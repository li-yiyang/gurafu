(in-package :gurafu/core)

;; ========== helper-functions ==========

(declaim (inline within-rectangle? rectangle-constrain))
(defun within-rectangle? (x y left right bottom top)
  "Test if point x, y is within rectangle. "
  (flet ((within? (a x b)               ; if a, b brackets x
           (if (< a b)
               (and (<= a x) (<= x b))
               (and (<= b x) (<= x a)))))
    (and (within? left x right)
         (within? bottom y top))))

(defun rectangle-constrain (x y left right bottom top)
  "Constrain point x, y within rectangle. "
  (flet ((constrain (a x b)             ; make x within a, b
           (if (< a b)
               (min (max a x) b)
               (min (max b x) a))))
    (values (constrain left x right)
            (constrain bottom y top))))

;; ========== coordinated-mixin ==========

(defclass coordinated-mixin ()
  ((%uv-left   :initform 0)
   (%uv-right  :initform 100)
   (%uv-bottom :initform 100)
   (%uv-top    :initform 0)
   
   (%xy-left   :initform -1 :initarg :x-min)
   (%xy-right  :initform 1  :initarg :x-max)
   (%xy-bottom :initform -1 :initarg :y-min)
   (%xy-top    :initform 1  :initarg :y-max))
  (:documentation
   "This class defines how the corrdinate is transformed,
which handles how the user set position `x', `y' is
transformed into digital plotting `u', `v'. Also, this
described how the coordinate axes is pointed to and
scaled.

By normal, the coordinate is like below:

  *-----------> u                   ^ y
  |        stream-pos!              |                 top
  | left  /                         |      +--------+ right
  | top  +--------+                 |      | stream |
  |      | stream | stream-height!  | left +--------+
  | v    +--------+    right        | bottom \ stream-xy-pos
  V      stream-width! bottom       *-------------------> x

"))

(defmethod initialize-instance :after
    ((stream coordinated-mixin)
     &key (width 100) (height 100) (left 0) (top 0))
  (set-stream-size     stream width height)
  (set-stream-position stream left  top))

;; ========== transformations ==========

(defgeneric xy-to-uv-translation (stream)
  (:documentation
   "Translation function from x, y to u, v.
Return a lambda function acceptting x, y as input,
output transformed u, v.

  x-scale = (uv-right - uv-left) / (xy-right - xy-left)
  y-scale = (uv-bottom - uv-top) / (xy-top - xy-bottom)

  u = uv-left + x-scale * (x - xy-left)
    => (uv-left - x-scale * xy-left) + x-scale * x
  v = uv-bottom - y-scale * (y - xy-bottom)
    => (uv-bottom + y-scale * xy-bottom) - y-scale * y
"))

(defgeneric xy-to-uv-translation (stream)
  (:documentation
   "Translation function from u, v to x, y.
Return a lambda function acceptting u, v as input,
output transformed x, y.

  u-scale = (xy-right - xy-left) / (uv-right - uv-left)
  v-scale = (xy-top - xy-bottom) / (uv-bottom - uv-top)

  x = xy-left + u-scale * (u - uv-left)
    => (xy-left - u-scale * uv-left) + u-scale * u
  y = xy-top - v-scale * (v - uv-top)
    => (xy-top + v-scale * uv-top) - v-scale * v
"))

(defmethod xy-to-uv-translation ((stream coordinated-mixin))
  (with-slots (%uv-left %uv-right %uv-bottom %uv-top
               %xy-left %xy-right %xy-bottom %xy-top)
      stream
    (let* ((x-scale (float (/ (- %uv-right %uv-left)
                              (- %xy-right %xy-left))))
           (y-scale (float (/ (- %uv-bottom %uv-top)
                              (- %xy-top %xy-bottom))))
           (u0 (- %uv-left   (truncate (* x-scale %xy-left))))
           (v0 (+ %uv-bottom (truncate (* y-scale %xy-bottom)))))
      (declare (integer u0 v0))
      (lambda (x y)
        (values (+ u0 (truncate (* x-scale x)))
                (+ v0 (truncate (* y-scale y))))))))

(defmethod uv-to-xy-translation ((stream coordinated-mixin))
  (with-slots (%uv-left %uv-right %uv-bottom %uv-top
               %xy-left %xy-right %xy-bottom %xy-top)
      stream
    (let* ((u-scale (float (/ (- %xy-right %xy-left)
                              (- %uv-right %uv-left))))
           (v-scale (float (/ (- %xy-top %xy-bottom)
                              (- %uv-bottom %uv-top))))
           (x0 (- %xy-left (* u-scale %uv-left)))
           (y0 (+ %xy-top  (* v-scale %uv-top))))
      (declare (float x0 y0))
      (lambda (u v)
        (values (+ x0 (* u-scale u))
                (+ y0 (* v-scale v)))))))

;; ========== stream-bounding-box ==========

(defgeneric stream-bounding-box (stream)
  (:documentation
   "Get the bounding box of `stream'.
Return values are inner edge left, right, bottom, top of uv box. "))

(defmethod stream-bounding-box ((stream coordinated-mixin))
  (with-slots (%uv-left %uv-right %uv-bottom %uv-top) stream
    (values %uv-left %uv-right %uv-bottom %uv-top)))

;; ========== stream-xy-box ==========

(defgeneric stream-xy-box (stream)
  (:documentation
   "Get the bounding box of `stream'.
Return values are left, right, bottom, top of xy box. "))

(defmethod stream-xy-box ((stream coordinated-mixin))
  (with-slots (%xy-left %xy-right %xy-bottom %xy-top) stream
    (values %xy-left %xy-right %xy-bottom %xy-top)))

;; ========== set-stream-bounding-box ==========

(defgeneric set-stream-bounding-box (stream left right bottom top)
  (:documentation
   "Set the bounding uv box of `stream'. "))

(defmethod set-stream-bounding-box ((stream coordinated-mixin)
                                    left right bottom top)
  (with-slots (%uv-left %uv-right %uv-bottom %uv-top) stream
    (setf %uv-left   left
          %uv-right  right
          %uv-bottom bottom
          %uv-top    top)))

;; ========== set-stream-xy-box ==========

(defgeneric set-stream-xy-box (stream left right bottom top)
  (:documentation
   "Set the xy box with left, right, bottom and top. "))

(defmethod set-stream-xy-box ((stream coordinated-mixin) left right bottom top)
  (with-slots (%xy-left %xy-right %xy-bottom %xy-top) stream
    (setf %xy-left   left
          %xy-right  right
          %xy-bottom bottom
          %xy-top    top)))

;; ========== set-stream-bounding-box-size ==========

(defgeneric set-stream-bounding-box-size (stream width height)
  (:documentation
   "Set the bounding box size. "))

(defmethod set-stream-bounding-box-size ((stream coordinated-mixin) width height)
  (multiple-value-bind (left right bottom top)
      (stream-bounding-box stream)
    (declare (ignore right bottom))
    (set-stream-bounding-box
     stream left (+ left width) (+ top height) top)))

;; ========== stream-position ==========

(defgeneric stream-position (stream)
  (:documentation
   "Get the uv position of `stream'. "))

(defmethod stream-position ((stream coordinated-mixin))
  (with-slots (%uv-left %uv-top) stream
    (values %uv-left %uv-top)))

;; ========== set-stream-position ==========

(defgeneric set-stream-position (stream uv-left uv-top)
  (:documentation
   "Set the `stream' position by `uv-left', `uv-top'. "))

(defmethod set-stream-position ((stream coordinated-mixin) uv-left uv-top)
  (with-slots (%uv-left %uv-right %uv-bottom %uv-top) stream
    (let ((uv-width  (- %uv-right  %uv-left))
          (uv-height (- %uv-bottom %uv-top)))
      (setf %uv-left uv-left
            %uv-right (+ uv-left uv-width)
            %uv-bottom (+ uv-top uv-height)
            %uv-top  uv-top))))

;; ========== stream-size ==========

(defgeneric stream-size (stream)
  (:documentation
   "Get the `stream' size.
Return width and height of stream. "))

(defmethod stream-size ((stream coordinated-mixin))
  (with-slots (%uv-left %uv-right %uv-bottom %uv-top) stream
    (values (- %uv-right %uv-left) (- %uv-bottom %uv-top))))

;; ========== set-stream-size ==========

(defgeneric set-stream-size (stream width height)
  (:documentation
   "Set the `stream' size. "))

(defmethod set-stream-size ((stream coordinated-mixin) width height)
  (with-slots (%uv-left %uv-right %uv-bottom %uv-top) stream
    (setf %uv-right  (+ %uv-left width)
          %uv-bottom (+ %uv-top  height))))
