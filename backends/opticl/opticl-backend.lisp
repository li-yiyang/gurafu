(in-package :gurafu/backends/opticl)

;; ========== opticl-utils ==========

(defparameter +ntsc-rgb-gray-weights+
  '(0.299 0.587 0.114)
  "The NTSC formula for RGB to Grayscale is:
  0.299 * R + 0.587 * G + 0.114 * B.")

(defparameter +opticl-support-format+
  '(:png :jpeg :jpg)
  "Opticl backend supported output format. ")

(defun check-opticl-format (format)
  "Check if `format' is supported by opticl backend. "
  (if (find format +opticl-support-format+)
      format
      (error (format nil "Unsupported format ~a. " format))))

;; ========== opticl-fonts ==========
;; Since there's no support for opticl font
;; drawing ability (as I know for now). So I
;; have to add it by myself.
;; 
;; Maybe later I shall improve the font part...
;; It's kinda slow reading font files right now.
;; For reading Unifont BDF fonts, it would take
;; me about 1.053 seconds, which may cause a little
;; stuck when loading the `:gurafu/backend/opticl'
;; system. That's why I split it out the
;; `opticl-backend' class and loaded just top-level.

(defparameter *opticl-bdf-fonts*
  (loop with font-table = (make-hash-table :test 'equal)
        for path in (uiop:directory-files
                     (asdf:system-relative-pathname :gurafu
                                                    "backends/opticl/fonts/"))
        if (string= (pathname-type path) "bdf")
          do (let ((font (string-upcase (pathname-name path))))
               (setf (gethash font font-table)
                     (load-bdf-font path)))
        finally (return font-table))
  "Opticl BDF fonts database.

This is a hash-table whose key is upper case string standing for
the name of BDF font, which is described by its path name (right now).

For example, the unifont in `fonts' dir is `unifont.bdf', so it would
be assigned to `UNIFONT' string for its key. ")

;; the `*opticl-default-font*' will be used as cloure value, mostly
(defparameter *opticl-default-font*
  (gethash "UNIFONT" *opticl-bdf-fonts*)
  "The default opticl font. ")

(declaim (inline find-font))
(defun find-font (font-name)
  "Find the font by its name.

Return values are `bdf' font in `*opticl-bdf-fonts*';
if fails to find, return `*opticl-default-font*'. "
  (or (gethash font-name *opticl-bdf-fonts* nil)
      *opticl-default-font*))

;; ========== opticl-backend ==========

(defclass opticl-backend (output-protocol)
  ((%opticl-image      :initform nil)
   (%opticl-colorspace :initform nil))
  (:documentation
   "GURAFU backend using opticl to render plot."))

;; ========== real-colorspace! ==========
;; This would UPDATE `opticl-colorspace' for opticl-backend

(defmethod real-colorspace-name! ((opticl opticl-backend))
  (slot-value opticl '%opticl-colorspace))

;; ========== rgb-color! ==========
;; The returned value is values for each color vector.
;; For example, `+white+' whould be mapped to `(values 255 255 255)'.

(defmethod rgb-color! ((opticl opticl-backend) rgb-color)
  (flet ((trun (v)
           (coerce (truncate (* v 255)) '(unsigned-byte 8))))
    (flet ((->8-bit-rgb ()
             (mapcar #'trun rgb-color))
           (->8-bit-gray ()
             (list
              (trun (apply #'+ (mapcar #'* rgb-color
                                       +ntsc-rgb-gray-weights+))))))
      (case (real-colorspace-name! opticl)
        (:8-bit-rgb  (->8-bit-rgb))
        (:8-bit-gray (->8-bit-gray))))))

;; ========== inititalize-instance ==========

(defmethod initialize-instance :after ((opticl opticl-backend) &key)
  (with-slots (%opticl-colorspace %opticl-image) opticl
    ;; set the `%opticl-colorspace'
    (setf %opticl-colorspace
          (ecase (colorspace! opticl)
            ((:rgb :8-bit-rgb) :8-bit-rgb)
            ((:gray :grayscale :grey :greyscale) :8-bit-gray)))
    ;; init the `%opticl-image'
    (setf %opticl-image
          (ecase %opticl-colorspace
            (:8-bit-rgb
             (make-8-bit-rgb-image
              (stream-height! opticl)
              (stream-width!  opticl)
              :initial-element
              (values-list (rgb-color! opticl *background-color*))))
            (:8-bit-gray
             (make-8-bit-gray-image
              (stream-height! opticl)
              (stream-width!  opticl)
              :initial-element
              (values-list (rgb-color! opticl *background-color*))))))))

;; ========== set-stream-width! ==========
;; ========== set-stream-height! ==========

(flet ((opticl-resize (opticl)
         (setf (slot-value opticl '%opticl-image)
               (resize-image %opticl-image
                             (stream-height! opticl)
                             (stream-width!  opticl)))))
  (defmethod (setf stream-width!) :after (width (opticl opticl-backend))
    (declare (ignore width))
    (opticl-resize opticl))
  
  (defmethod (setf stream-height!) :after (height (opticl opticl-backend))
    (declare (ignore height))
    (opticl-resize opticl)))


;; ========== output! ==========

(defmethod output! ((opticl opticl-backend)
                    (path pathname)
                    &key (format :png format-set?))
  (let ((format (if format-set? format
                    (make-keyword
                     (format nil "~:@(~a~)" (pathname-type path))))))
    (with-slots (%opticl-image) opticl
      (case format
        (:png         (write-png-file  path %opticl-image))
        ((:jpg :jpeg) (write-jpeg-file path %opticl-image))
        (otherwise
         (error
          (format nil "Format ~a is not supported yet." format)))))))

(defmethod output! ((opticl opticl-backend)
                    (path string)
                    &key (format :png format-set?))
  (if format-set?
      (output! opticl (pathname path) :format format)
      (output! opticl (pathname path))))

;; ========== draw-pixel! ==========

(defmethod draw-pixel! ((opticl opticl-backend) u v color)
  (with-slots (%opticl-image) opticl
    (with-image-bounds (height width) %opticl-image
      (when (and (<= 0 u) (< u width)
                 (<= 0 v) (< v height))
        (set-pixels (u v) %opticl-image
          (values-list (rgb-color! color)))))))

;; ========== draw-point! ==========

(defmethod draw-point! ((opticl opticl-backend) u v
                        &key
                          (color +black+)
                          (pen-width 1)
                          (point-style :dot)
                        &allow-other-keys)
  (let ((img   (slot-value opticl '%opticl-image))
        (size  (ceiling pen-width 2))
        (color (rgb-color! opticl color)))
    (ecase point-style
      ((:dot :point)
       (apply #'fill-circle img v u size color))
      ((:plus :+)
       (apply #'draw-line img
              (- v size) u
              (+ v size) u
              color)
       (apply #'draw-line img
              v (- u size)
              v (+ u size)
              color))
      ((:cross :times :x)
       (apply #'draw-line img
              (- v size) (- u size)
              (+ v size) (+ u size)
              color)
       (apply #'draw-line img
              (+ v size) (- u size)
              (- v size) (+ u size)
              color))
      ((:box :rect :square)
       (apply #'draw-rectangle img
              (- v size) (- u size)
              (+ v size) (+ u size)
              color))
      ((:circle :O)
       (apply #'draw-circle img v u size color))
      ((:triangle)
       (apply #'draw-triangle img
              (- v size) u
              (+ v size) (+ u size)
              (+ v size) (- u size)
              color)))
    ;; return the binding box coordinates
    (values (- u size) (+ u size) (+ v size) (- v size))))

;; ========== draw-line! ==========

(defmethod draw-line! ((opticl opticl-backend) u1 v1 u2 v2
                       &key (color +black+)
                       ;; (line-style :solid) not implemented
                         (pen-width 1 pen-width-set?)
                       &allow-other-keys)
  (let ((img   (slot-value opticl '%opticl-image))
        (color (rgb-color! opticl color)))
    ;; draw the line
    (apply #'draw-line img v1 u1 v2 u2 color)

    ;; if set pen-width
    ;; poor man's `pen-width' implementation
    (when pen-width-set?
      (let ((half-w (truncate pen-width 2)))
        (if (> (abs (- u2 u1)) (abs (- v2 v1)))
            (dotimes (offset (truncate pen-width 2))
              (apply #'draw-line
                     img (+ v1 offset 1) u1 (+ v2 offset 1) u2 color)
              (apply #'draw-line
                     img (- v1 offset 1) u1 (- v2 offset 1) u2 color))
            (dotimes (offset (truncate pen-width 2))
              (apply #'draw-line
                     img v1 (+ u1 offset 1) v2 (+ u2 offset 1) color)
              (apply #'draw-line
                     img v1 (- u1 offset 1) v2 (- u2 offset 1) color)))))

    ;; return the bounding box
    (values (min u1 u2) (max u1 u2)
            (min v1 v2) (max v1 v2))))

;; ========== draw-circle! ==========

(defmethod draw-circle! ((opticl opticl-backend) u v uv-r
                         &key (color +black+)
                           (pen-width 1)
                         ;; (line-style :solid) not implemented
                           (fill? t)
                           (fill-color color)
                         &allow-other-keys)
  (with-slots (%opticl-image) opticl
    ;; poor man's pen-width... really ugly... 
    (let ((color (rgb-color! opticl color))
          (r (+ pen-width uv-r -1)))
      (dotimes (i pen-width)
        (apply #'draw-circle %opticl-image
               v u (+ uv-r i) color))
      (when fill?
        (apply #'fill-circle %opticl-image
               v u uv-r (rgb-color! opticl fill-color)))
      (values (- u r) (+ u r) (+ v r) (- v r)))))

;; ========== draw-rect! ==========

(defmethod draw-rect! ((opticl opticl-backend) u1 v1 u2 v2
                       &key (color *foreground-color*)
                         (pen-width 1)
                         (line-style :solid)
                         (fill? t)
                         (fill-color color)
                       &allow-other-keys)
  (let ((pen-w (truncate pen-width 2)))

    ;; fill the rectangle
    (when fill?
      (apply #'fill-rectangle (slot-value opticl '%opticl-image)
             v1 u1 v2 u2 (rgb-color! opticl fill-color)))

    ;; draw boundary
    (draw-line! opticl u1 v1 u1 v2
                :line-style line-style
                :pen-width pen-width
                :color color)
    (draw-line! opticl u2 v1 u2 v2
                :line-style line-style
                :pen-width pen-width
                :color color)
    (draw-line! opticl u1 v1 u2 v1
                :line-style line-style
                :pen-width pen-width
                :color color)
    (draw-line! opticl u1 v2 u2 v2
                :line-style line-style
                :pen-width pen-width
                :color color)

    ;; return bounding rectangle
    (values (- u1 pen-w) (+ u2 pen-w) (+ v2 pen-w) (- v1 pen-w))))

;; ========== draw-triangle! ==========

(defmethod draw-triangle! ((opticl opticl-backend) u1 v1 u2 v2 u3 v3
                           &key (color *foreground-color*)
                             (pen-width 1)
                             (line-style :solid)
                             ;; (fill? t) not implemented
                             ;; (fill-color color)
                           &allow-other-keys)

  ;; draw triangle
  (draw-line! opticl u1 v1 u2 v2
              :line-style line-style
              :pen-width pen-width
              :color color)
  (draw-line! opticl u1 v1 u3 v3
              :line-style line-style
              :pen-width pen-width
              :color color)
  (draw-line! opticl u2 v2 u3 v3
              :line-style line-style
              :pen-width pen-width
              :color color)

  ;; return bounding box
  (values (min u1 u2 u3) (max u1 u2 u3)
          (min v1 v2 v3) (max v1 v2 v3)))

;; ========== draw-char ==========
;; This is an extension for opticl image to
;; draw character on image.

(declaim (inline char-size))
(defun char-width (char scale)
  "Get the width of `char' when drawing with `scale'.
Return values are bounding box of char width. "
  (values (truncate (* scale (font-size char)))))

(defun text-size (char-list font-size char-forward 
                  &optional (line-width 0 line-width-set?)
                    (line-forward '(0 1.5))
                    (font *opticl-default-font*))
  "Calculate the size of `text' when drawing with `scale'.

The `font' is a `bdf' object standing for drawing font;
The `font-size' is the height of the drawing char;
The `line-forward' is a vector length for char spacing,
  forwarding to next char;
The `line-forward' is a vector length for line spacing,
  forwarding to next line (must provide if set `line-width')

Return values are bounding box of text width and height. 

You could feed in `line-width' to limit the max width of a text line,
if not given `line-width', default assuming infinite long line. "
  (loop with scale = (font-scale font font-size)

        ;; left, right, bottom, top is the bounding box size
        with left = 0 with right = 0
        with bottom = 0 with top = 0

        ;; u, v is the char cursor position
        with u = 0 with v = 0

        ;; line-u, line-v is the line start position
        with line-u = 0 with line-v = 0

        ;; forward-u and forward-v is the char moving scale vector
        with forward-u = (first char-forward)
        with forward-v = (second char-forward)

        ;; forline-u, forline-v is the line return moving vector
        with forline-u = (truncate (* font-size
                                      (or (first  line-forward) 0)))
        with forline-v = (truncate (* font-size
                                      (or (second line-forward) 0)))

        for c in char-list
        for char  = (get-char font
                              (format nil "U+~4,'0X" (char-code c)))
        for width = (char-width char scale)

        ;; update bounding box
        do (setf left   (min left   u)
                 right  (max right  (+ u width))
                 bottom (max bottom (+ v font-size))
                 top    (min top    v))

        ;; move cursor to next char position
        do (incf u (truncate (* width forward-u)))
        do (incf v (truncate (* width forward-v)))

        ;; move to next line if overflow line-width
        if (or (equal c #\Newline)
               (and line-width-set?
                    (> u line-width)))
          do (setf line-u (+ line-u forline-u)
                   line-v (+ line-v forline-v))
          and do (setf u line-u
                       v line-v)

        ;; return the bounding box size
        finally (return (values (abs (- right left))
                                (abs (- bottom top))))))

;; (text-size (map 'list #'identity "This is how I work")
;;            *opticl-default-font*
;;            16 '(1 0) 100)

(defun draw-char (image u v char scale color)
  "Draw a char on image at position `u' and `v'.

The `char' is a `char' object, for example `#\A', `#\B' and so on.
The `font-size' is the height of the char.
The `color' is the color list which will be truned into values-list.

Return values are shifted position of u and v for next char.

To draw a char:
       u, v: the origin point
      /
    *-----------+----->*
          bbx   |       \
                | -bby   u + dwx, v: shifted point
                V
                *##OOOO
               / ##OOOO
 shifted-origin  ##OOOO
                 ######
                 ######
                  font-bitmap
"
  (when char
    (multiple-value-bind (bbw bbh bbx bby)
        (font-bounding-box char)
      (loop with pix = (ceiling scale) ; pixel size               
            with u0 = (truncate (+ u (* scale bbx)))
            with v0 = (truncate (+ v (* scale bby)))

            for bit-row in (font-bitmap char)
            for row below bbh
            for j from v0 by pix
            do (loop for bit in bit-row
                     for col below bbw
                     for i from u0 by pix
                     if (= bit 1)
                       do (apply #'fill-rectangle image
                                 j i (+ j pix -1) (+ i pix -1) color))
               
            finally (return
                      (values
                       (ceiling (+ u (* (font-size char) scale) pix))
                       v))))))



(text-size (map 'list #'identity "adfasdf
asdfasfdasfdsafafdsfas")
           16
           '(1.0 0.0))

;; ========== draw-text! ==========

(declaim (inline norm-list num-list 2d-rotate-clockwise))
(defun norm-list (list)
  "Calculate the norm length of list. "
  (apply #'+ (mapcar #'* list list)))

(defun num-list (num list)
  "Multiple list element with num. "
  (mapcar (lambda (x) (* num x)) list))

(defun 2d-rotate-clockwise (list)
  "Rotate a 2d list 90 degree clockwise. "
  (let ((x (first list))
        (y (second list)))
    (list y (- x))))

;; TODO: Make this function more shorter and easy to debug...
(defmethod draw-text! ((opticl opticl-backend) u v text
                       &key (color *foreground-color*)
                         (text-path '(1.0 0.0))
                         (text-align :normal)
                         (font-size 16)
                         (font-name "UNIFONT")
                         (char-spacing 1.0)
                         (line-width 100 line-width-set?)
                         (line-spacing 1.5)
                       &allow-other-keys)
  (let* ((char-list (map 'list #'identity text))
         (*opticl-default-font* (find-font font-name))
         (forward (num-list (float (/ 1 (norm-list text-path)))
                            text-path))
         (char-forward (num-list char-spacing forward))
         (line-forward (num-list line-spacing
                                 (2d-rotate-clockwise forward)))
         (color (rgb-color! opticl color))
         (img (slot-value opticl '%opticl-image)))
    (multiple-value-bind (width height)
        (if line-width-set?
            (text-size char-list font-size char-forward
                       line-width line-forward)
            (text-size char-list font-size char-forward))
      (loop with scale = (font-scale *opticl-default-font* font-size)

            with (u0 v0) = (ecase text-align
                             ((:normal
                               :left
                               :left-top
                               :top-left)
                              (list u v))
                             ((:center
                               :horizontal-center
                               :top-center)
                              (list (- u (truncate width 2)) v))
                             ((:vertical-center
                               :left-vertical-center
                               :vertical-center-left)
                              (list u (- v (truncate height 2))))
                             ((:right
                               :right-top
                               :top-right)
                              (list (- u width) v)))

            with (cursor-u cursor-v) = (list u0 v0)
            with (line-u   line-v)   = (list u0 v0)

            with (forward-u forward-v) = char-forward

            with (forline-u forline-v) = (mapcar #'truncate
                                                 (num-list font-size
                                                           line-forward))

            for c in char-list
            for char = (get-char *opticl-default-font*
                                 (format nil "U+~4,'0X" (char-code c)))
            for width = (char-width char scale)

            do (draw-char img cursor-u cursor-v char scale color)
            do (incf cursor-u (truncate (* width forward-u)))
            do (incf cursor-v (truncate (* width forward-v)))

            if (or (equal c #\Newline)
                   (and line-width-set?
                        (> cursor-u line-width)))
              do (setf line-u (+ line-u forline-u)
                       line-v (+ line-v forline-v))
              and do (setf u line-u
                           v line-v)))))
