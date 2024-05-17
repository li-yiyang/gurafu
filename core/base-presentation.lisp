(in-package :gurafu/core)

;; ========== helper functions ==========

;; ========== +gurafu-backends+ ==========
;; Should regist all the GURAFU backend here...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +gurafu-backends+
    '(((:opticl-backend :opticl)
       gurafu/backends/opticl:opticl-backend))
    "The GURAFU backends. "))

(defmacro make-backend (type &key (colorspace :8-bit-rgb)
                               (height 100) (width 100))
  "Make a GURAFU backend. "
  `(ecase ,type
     ,@(loop for (nicknames type) in +gurafu-backends+
             collect `(,nicknames
                       (make-instance ',type
                                      :colorspace ,colorspace
                                      :height ,height :width ,width)))))

;; by default, the `*default-backend*' should be a small `:opticl' backend
;; the size should be resized when using it. 
(defparameter *default-backend*
  (make-backend :opticl :width 1 :height 1)
  "The default backend used for GURAFU plotting lib. ")

;; ========== base-presentation ==========

(defclass base-presentation (coordinated-box)
  ((%backend :initform *default-backend* :initarg :backend))
  (:documentation
   "This is the base class for GURAFU presentation object.

When initialize the `base-presentation' objects, the default `%backend'
will be the `*default-backend*'. It could be overwritten by closure
variables.

The `base-presentation' objects should implement it's `present' method.
To do this, you should use `draw-*' method like `draw-line', `draw-rect'.

The `base-presentation' is a `coordinated-box', so when drawing with
`draw-*' methods, the used coordinates could be:
+ 0 ~ 1: which would be mapped to ratio corresponding to coordinated-box;
+ others (greater than 1): which be position relative to left, top.

But please NOTE that the `pen-width', `font-size' etc specifing params
are absolute units, which would not be translated. 

Note that you could also use `%backend' slot and `draw-*!' method,
which is quite low-level... Not recommanded. "))

(defmethod initialize-instance :after
    ((present base-presentation)
     &key backend (width 100) (height 100) (colorspace :8-bit-rgb))
  (with-slots (%backend) present
    (unless (typep %backend 'base-protocol)      
      (setf %backend (make-backend backend
                                   :colorspace colorspace
                                   :width      width
                                   :height     height)))))

;; ========== present ==========

(defgeneric present (obj)
  (:documentation
   "To present the `obj' via its backend. "))

(defmethod present (obj)
  (declare (ignore obj)))

;; ========== draw-present ==========

(defun draw-present (obj left right bottom top)
  "Present `obj' on coordinates. "
  (set-stream-bounding-box obj left right bottom top)
  (present obj))

;; ========== with-present ==========

(defmacro with-present ((var type &rest init-args) &body body)
  "Quickly make a present, return present. "
  `(let* ((,var (make-instance ',type ,@init-args)))
     ,@body
     ,var))

;; ========== with-present-to-file ==========

(defmacro with-present-to-file ((var type &rest init-args)
                                (file &key (width 400) (height 400)
                                        (backend :opticl) (colorspace :8-bit-rgb))
                                &body body)
  "Dirty quick presentations. "
  `(let* ((*default-backend* (make-backend ,backend
                                           :width  ,width
                                           :height ,height
                                           :colorspace ,colorspace))
          (,var (make-instance ',type ,@init-args)))
     (set-stream-bounding-box ,var 0 ,width ,height 0)
     ,@body
     (present ,var)
     (output! *default-backend* ,file)))

;; ========== draw-functions ==========

(defgeneric draw-point (obj x y
                        &key color point-style pen-width
                        &allow-other-keys)
  (:documentation "Draw text on `obj', position relative to `obj'. "))

(defgeneric draw-text-size (obj text
                            &key text-path char-forward line-forward
                              font-size font-name char-spacing
                              line-width line-spacing
                            &allow-other-keys)
  (:documentation "Get text size when drawing on `obj'. "))

(defgeneric draw-text (obj x y text
                       &key color
                         text-path char-forward line-forward
                         text-align font-size font-name char-spacing
                         line-width line-spacing
                       &allow-other-keys)
  (:documentation "Draw text on `obj', position relative to `obj'. "))

(defgeneric draw-triangle (obj x1 y1 x2 y2 x3 y3
                           &key color pen-width line-style
                             fill? fill-color
                           &allow-other-keys)
  (:documentation "Draw triangle on `obj', position relative to `obj'. "))

(defgeneric draw-rect (obj x1 y1 x2 y2
                       &key color pen-width line-style
                         fill? fill-color
                       &allow-other-keys)
  (:documentation "Draw rectangle on `obj', position relative to `obj'. "))

(defgeneric draw-circle (obj x y r
                         &key color pen-width line-style
                           fill? fill-color
                         &allow-other-keys)
  (:documentation "Draw circle on `obj', position relative to `obj'. "))

(defgeneric draw-line (obj x1 y1 x2 y2
                       &key color line-style pen-width
                       &allow-other-keys)
  (:documentation "Draw line on `obj', position relative to `obj'. "))

;; ========== draw-functions implementation ==========

(declaim (inline coord))
(defun coord (left x right)
  (if (and (floatp x) (<= 0.0 x) (<= x 1.0))
      (truncate (+ left (* x (- right left))))
      (truncate (+ left x))))

(defmethod draw-point ((obj base-presentation) x y
                       &key (color *foreground-color*)
                         (point-style *point-style*)
                         (pen-width *pen-width*)
                         (offset '(0 0))
                       &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let ((x (+ (coord left x right)  (first offset)))
          (y (+ (coord top  y bottom) (second offset)))
          (*point-style* point-style)
          (*pen-width*   pen-width)
          (*foreground-color* color))
      (draw-point! (slot-value obj '%backend) x y))))

(defmethod draw-text-size ((obj base-presentation) text
                           &key (text-path '(1.0 0.0) text-path-set?)
                             (line-forward *line-forward*)
                             (char-forward *char-forward*)
                             (font-size *font-size*)
                             (font-name "UNIFONT")
                             (char-spacing *char-spacing*)
                             (line-width 0)
                             (line-spacing *line-spacing*)
                           &allow-other-keys)
  (let ((*line-forward* (if text-path-set?
                            (reverse text-path) ;; rotate 90 degree clockwise
                            line-forward))
        (*char-forward* (if text-path-set?
                            text-path
                            char-forward))
        (*line-spacing* line-spacing)
        (*char-spacing* char-spacing)
        (*font-size*    font-size))
    (draw-text-size! (slot-value obj '%backend) text
                     :font-name font-name
                     :line-width line-width)))

(defmethod draw-text ((obj base-presentation) x y text
                      &key (color *foreground-color*)
                        (text-path '(1.0 0.0) text-path-set?)
                        (char-forward *char-forward*)
                        (line-forward *line-forward*)
                        (text-align *text-align*)
                        (font-size  *font-size*)
                        (font-name "UNIFONT")
                        (char-spacing *char-spacing*)
                        (line-width 0)
                        (line-spacing *line-spacing*)
                        (offset '(0 0))
                      &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let ((x (+ (coord left x right)  (first offset)))
          (y (+ (coord top  y bottom) (second offset))))
      (let ((*line-forward* (if text-path-set?
                                (reverse text-path) ;; rotate 90 degree clockwise
                                line-forward))
            (*char-forward* (if text-path-set?
                                text-path
                                char-forward))
            (*line-spacing* line-spacing)
            (*char-spacing* char-spacing)
            (*font-size*    font-size)
            (*foreground-color* color)
            (*text-align* text-align))
        (draw-text! (slot-value obj '%backend) x y text
                    :font-name    font-name
                    :line-width   line-width)))))

(defmethod draw-triangle ((obj base-presentation) x1 y1 x2 y2 x3 y3
                          &key (color *foreground-color*)
                            (pen-width *pen-width*)
                            (line-style *line-style*)
                            (fill? *fill?*)
                            (fill-color color)
                            (offset '(0 0))
                          &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let* ((offx (first offset))
           (offy (second offset))
           (x1 (+ (coord left x1 right) offx))
           (y1 (+ (coord top  y1 bottom) offy))
           (x2 (+ (coord left x2 right) offx))
           (y2 (+ (coord top  y2 bottom) offy))
           (x3 (+ (coord left x3 right) offx))
           (y3 (+ (coord top  y3 bottom) offy))
           (*fill?* fill?)
           (*foreground-color* color)
           (*pen-width* pen-width)
           (*line-style* line-style))
      (draw-tringle! (slot-value obj '%backend)
                     x1 y1 x2 y2 x3 y3
                     :fill-color fill-color))))

(defmethod draw-rect ((obj base-presentation) x1 y1 x2 y2
                      &key (color *foreground-color*)
                        (pen-width *pen-width*)
                        (line-style *line-style*)
                        (fill? *fill?*)
                        (fill-color color)
                        (offset '(0 0))
                      &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let* ((offx (first offset))
           (offy (second offset))
           (x1 (+ (coord left x1 right) offx))
           (y1 (+ (coord top  y1 bottom) offy))
           (x2 (+ (coord left x2 right) offx))
           (y2 (+ (coord top  y2 bottom) offy))
           (*fill?* fill?)
           (*line-style* line-style)
           (*pen-width* pen-width)
           (*foreground-color* color))
      (draw-rect! (slot-value obj '%backend)
                  (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)
                  :fill-color fill-color))))

(defmethod draw-line ((obj base-presentation) x1 y1 x2 y2
                      &key (color *foreground-color*)
                        (pen-width *pen-width*)
                        (line-style *line-style*)
                        (offset '(0 0))
                      &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let* ((offx (first offset))
           (offy (second offset))
           (x1 (+ (coord left x1 right) offx))
           (y1 (+ (coord top  y1 bottom) offy))
           (x2 (+ (coord left x2 right) offx))
           (y2 (+ (coord top  y2 bottom) offy))
           (*foreground-color* color)
           (*pen-width* pen-width)
           (*line-style* line-style))
      (draw-line! (slot-value obj '%backend) x1 y1 x2 y2))))
