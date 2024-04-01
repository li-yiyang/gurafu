(in-package :gurafu/backends/opticl)

;; ========== opticl-utils ==========

(defconstant +ntsc-rgb-gray-weights+
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
    (setf %opticl-colorspace
          (ecase (slot-value opticl 'colorspace)
            ((:rgb :8-bit-rgb) :8-bit-rgb)
            ((:gray :grayscale :grey :greyscale) :8-bit-gray)))
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

;; ========== draw-point! ==========

(defmethod draw-point! ((opticl opticl-backend) u v
                        &key
                          (color +black+)
                          (pen-width 1)
                        &allow-other-keys)
  (with-slots (opticl-image) opticl
    (apply #'fill-circle opticl-image v u pen-width
           (rgb-color! opticl color))))

;; ========== draw-line! ==========

(defmethod draw-line! ((opticl opticl-backend) u1 v1 u2 v2
                       &key
                         (color +black+)
                         (pen-width 1)
                       &allow-other-keys)
  ;; No pen-width yet
  (with-slots (opticl-image) opticl
    (let ((color (rgb-color! opticl color)))
      ;; poor man's `pen-width', need rewrite in future
      (dotimes (offset (truncate pen-width 2))
        (apply #'draw-line opticl-image
               (+ v1 offset) u1 (+ v2 offset) u2 color)
        (apply #'draw-line opticl-image
               (- v1 offset) u1 (- v2 offset) u2 color))
      (apply #'draw-line opticl-image v1 u1 v2 u2 color))))

;; ========== draw-circle! ==========

(defmethod draw-circle! ((opticl opticl-backend) u v uv-r
                         &key
                           (color +black+)
                           (pen-width 1)
                           (fill? t)
                           (fill-color color))
  (with-slots (opticl-image) opticl
    (let ((color (rgb-color! opticl color)))
      (dotimes (i pen-width)
        (apply #'draw-circle opticl-image v u (+ uv-r i) color)))
    (when fill?
      (apply #'fill-circle opticl-image v u uv-r
             (rgb-color! opticl fill-color)))))

;; ========== draw-rect! ==========

(defmethod draw-rect! ((opticl opticl-backend) u1 v1 u2 v2
                       &key (color *foreground-color*)
                         (pen-width 1)
                         (fill? t)
                         (fill-color color)
                       &allow-other-keys)
  (with-slots (opticl-image) opticl
    (let ((color (rgb-color! opticl color)))
      (dotimes (i pen-width)
        (apply #'draw-rectangle
               opticl-image
               (- v1 i) (- u1 i) (+ v2 i) (+ u2 i)
               color)))
    (when fill?
      (apply #'fill-rectangle
             opticl-image
             v1 u1 v2 u2
             (rgb-color! opticl fill-color)))))

;; ========== draw-text! ==========

(defmethod draw-text! ((opticl opticl-backend) u v text
                       &key (font-size 12)
                         (font-align :center)
                         (font-family "CPTFONT")
                         (font-sytle :normal)
                         rotation
                       &allow-other-keys)
  (declare (ignore rotation font-size font-align))
  (loop with char-u = u
        with char-v = v
        with font = (find-font opticl
                               :font-style font-sytle
                               :font-family font-family)
        
        for char-code in (map 'list #'char-code text)
        for char = (format nil "U+~4,'0X" char-code)
        do (multiple-value-setq (char-u char-v)
             (draw-char! opticl char-u char-v char font))))

;; ========== draw-char! ==========

(defgeneric draw-char! (opticl u v char font)
  (:documentation
   "Draw character `char' on `opticl' stream at position `u', `v'.
Return value are shifted position for char. "))


(defmethod draw-char! (opticl u v char font)
  (let ((char (get-char font char)))
    (when char
      (multiple-value-bind (bbw bbh bbx bby)
          (font-bounding-box char)
        (loop with img = (slot-value opticl 'opticl-image)
              with color = (rgb-color! opticl +black+)
              for bit-row in (font-bitmap char)
              for row below bbh
              do (loop for bit in bit-row
                       for col below bbw
                       for ui = (+ u bbx col)
                       for vi = (+ v (- bby) row)
                       if (= bit 1)
                         do (setf (pixel img vi ui)
                                  (values-list color))))
        (values (+ u bbw) v)))))
