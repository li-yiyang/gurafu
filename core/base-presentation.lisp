(in-package :gurafu/core)

;; ========== helper functions ==========

(defparameter +gurafu-backends+
  '(((:opticl-backend :opticl)
     gurafu/backends/opticl:opticl-backend))
  "The GURAFU backends. ")

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
;; the size would be resized when using it. 
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

;; ========== draw-functions ==========

(defgeneric draw-point (obj x y
                        &key color point-style pen-width
                        &allow-other-keys)
  (:documentation "Draw text on `obj', position relative to `obj'. "))

(defgeneric draw-text (obj x y text
                       &key color text-path text-align
                         font-size font-name char-spacing
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
  (if (and (<= 0.0 x) (<= x 1.0))
      (truncate (+ left (* x (- right left))))
      (truncate (+ left x))))

(defmethod draw-point ((obj base-presentation) x y
                       &key (color *foreground-color*)
                         (point-style :dot)
                         (pen-width 1)
                       &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let ((x (coord left x right))
          (y (coord top  y bottom)))
      (draw-point! (slot-value obj '%backend) x y
                   :point-style point-style
                   :pen-width pen-width
                   :color color))))

(defmethod draw-text ((obj base-presentation) x y text
                      &key (color *foreground-color*)
                        (text-path '(1.0 0.0))
                        (text-align :normal)
                        (font-size 16)
                        (font-name "UNIFONT")
                        (char-spacing 1.0)
                        (line-width 0 line-width-set?)
                        (line-spacing 1.5)
                      &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let ((x (coord left x right))
          (y (coord top  y bottom)))
      (with-slots (%backend) obj
        ;; I hate this, must change this later...
        (if line-width-set?
            (draw-text! %backend x y text
                        :color        color
                        :text-path    text-path
                        :text-align   text-align
                        :font-size    font-size
                        :font-name    font-name
                        :char-spacing char-spacing
                        :line-width   line-width
                        :line-spacing line-spacing)
            (draw-text! %backend x y text
                        :color        color
                        :text-path    text-path
                        :text-align   text-align
                        :font-size    font-size
                        :font-name    font-name
                        :char-spacing char-spacing
                        :line-spacing line-spacing))))))

(defmethod draw-triangle ((obj base-presentation) x1 y1 x2 y2 x3 y3
                          &key (color *foreground-color*)
                            (pen-width 1)
                            (line-style :solid)
                            (fill? t)
                            (fill-color color)
                          &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let ((x1 (coord left x1 right))
          (y1 (coord top  y1 bottom))
          (x2 (coord left x2 right))
          (y2 (coord top  y2 bottom))
          (x3 (coord left x3 right))
          (y3 (coord top  y3 bottom)))
      (draw-tringle! (slot-value obj '%backend)
                     x1 y1 x2 y2 x3 y3
                     :fill-color fill-color
                     :fill? fill?
                     :line-style line-style
                     :pen-width pen-width
                     :color color))))

(defmethod draw-rect ((obj base-presentation) x1 y1 x2 y2
                      &key (color *foreground-color*)
                        (pen-width 1)
                        (line-style :solid)
                        (fill? t)
                        (fill-color color)
                      &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let ((x1 (coord left x1 right))
          (y1 (coord top  y1 bottom))
          (x2 (coord left x2 right))
          (y2 (coord top  y2 bottom)))
      (draw-rect! (slot-value obj '%backend)
                  x1 y1 x2 y2
                  :line-style line-style
                  :pen-width pen-width
                  :color color
                  :fill? fill?
                  :fill-color fill-color))))

(defmethod draw-line ((obj base-presentation) x1 y1 x2 y2
                      &key (color *foreground-color*)
                        (pen-width 1)
                        (line-style :solid)
                      &allow-other-keys)
  (multiple-value-bind (left right bottom top)
      (stream-box obj)
    (let ((x1 (coord left x1 right))
          (y1 (coord top  y1 bottom))
          (x2 (coord left x2 right))
          (y2 (coord top  y2 bottom)))
      (draw-line! (slot-value obj '%backend)
                  x1 y1 x2 y2
                  :line-style line-style
                  :pen-width pen-width
                  :color color))))
