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

(defclass opticl-backend (output-protocol
                          bdf-font-mixin)
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
          (ecase (colorspace opticl)
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
                        &allow-other-keys)
  (with-slots (%opticl-image) opticl
    (let ((r (ceiling pen-width 2)))
      (apply #'fill-circle %opticl-image v u r (rgb-color! opticl color))
      (values (- u r) (+ u r) (+ v r) (- v r)))))

;; ========== draw-line! ==========

(defmethod draw-line! ((opticl opticl-backend) u1 v1 u2 v2
                       &key
                         (color +black+)
                         (pen-width 1)
                       &allow-other-keys)
  ;; No pen-width yet
  (with-slots (%opticl-image) opticl
    (let ((color (rgb-color! opticl color))
          (half-w (truncate pen-width 2)))
      ;; poor man's `pen-width', need rewrite in future
      (dotimes (offset half-w)
        (apply #'draw-line %opticl-image
               (+ v1 offset) u1 (+ v2 offset) u2 color)
        (apply #'draw-line %opticl-image
               (- v1 offset) u1 (- v2 offset) u2 color))
      (apply #'draw-line %opticl-image v1 u1 v2 u2 color)
      (values (min u1 u2) (max u1 u2)
              (- (min v1 v2) hafl-w) (+ (max v1 v2) half-w)))))

;; ========== draw-circle! ==========

(defmethod draw-circle! ((opticl opticl-backend) u v uv-r
                         &key
                           (color +black+)
                           (pen-width 1)
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
                         (fill? t)
                         (fill-color color)
                       &allow-other-keys)
  (with-slots (%opticl-image) opticl
    (let ((color (rgb-color! opticl color))
          (pen-w (- pen-width 1)))
      (dotimes (i pen-width)
        (apply #'draw-rectangle %opticl-image
               (- v1 i) (- u1 i) (+ v2 i) (+ u2 i) color))
      (when fill?
        (apply #'fill-rectangle %opticl-image
               v1 u1 v2 u2 (rgb-color! opticl fill-color)))
      (values (- u1 pen-w) (+ u2 pen-w) (+ v2 pen-w) (- v1 pen-w)))))

;; ========== draw-char ==========
;; This is an extension for opticl image to
;; draw character on image.

(defun draw-char (image u v char scale color
                   &optional (font *opticl-default-font*))
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
  (let ((char (get-char font (format nil "U+~4,'0X" (char-code char)))))
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
                 
              finally (return (values (ceiling (+ u (* (font-size char) scale) pix))
                                      v)))))))

;; ========== draw-text! ==========

(defmethod draw-text! ((opticl opticl-backend) u v text
                       &key (color *foreground-color*)
                         (font-size 12)
                         (font-family "UNIFONT")
                         (max-text-width 100 max-text-width-set?)
                         (line-spacing 1.5)
                       &allow-other-keys)
  (loop with char-u = u
        with char-v = v
        with font = (find-font font-family)
        with image = (slot-value opticl '%opticl-image)
        with color = (rgb-color! opticl color)
        with scale = (font-scale font font-size)

        ;; for each char in `text'
        for i below (length text)
        for char = (char text i)

        ;; draw char and rebound the char origin `char-u' and `char-v'
        do (multiple-value-setq (char-u char-v)
             (draw-char image char-u char-v char
                        scale color font))

        ;; move to next line if char origin outside `max-text-width'
        if (and max-text-width-set?
                (> (- char-u u) max-text-width))
          do (setf char-u u
                   char-v (+ char-v (* font-size line-spacing)))

        do ))
