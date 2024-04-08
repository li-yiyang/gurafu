(in-package :gurafu/core)

;; ========== helper-functions ==========

(declaim (inline within-rectangle? constrain rectangle-constrain))
(defun within-rectangle? (x y left right bottom top)
  "Test if point x, y is within rectangle. "
  (flet ((within? (a x b)               ; if a, b brackets x
           (if (< a b)
               (and (<= a x) (<= x b))
               (and (<= b x) (<= x a)))))
    (and (within? left x right)
         (within? bottom y top))))

(defun constrain (a x b)
  "Constrain point x within a and b. "
  (if (< a b) (min (max a x) b) (min (max b x) a)))

(defun rectangle-constrain (x y left right bottom top)
  "Constrain point x, y within rectangle. "
  (values (constrain left x right)
          (constrain bottom y top)))

;; ========== coordinated-box ==========

(defclass coordinated-box ()
  ((%uv-left   :initform 0   :initarg :left)
   (%uv-right  :initform 100 :initarg :right)
   (%uv-bottom :initform 100 :initarg :bottom)
   (%uv-top    :initform 0   :initarg :top))
  (:documentation
   "A box dealing with screen "))

;; ========== stream-box ==========

(defgeneric stream-box (stream)
  (:documentation
   "Get the box of `stream'.

Return values are `%uv-left', `%uv-right', `%uv-bottom' and `%uv-right'. "))

(defmethod stream-box ((box coordinated-box))
  (with-slots (%uv-left %uv-right %uv-bottom %uv-top) box
    (values %uv-left %uv-right %uv-bottom %uv-top)))

;; ========== set-stream-box ==========

(defgeneric set-stream-box (stream left right bottom top)
  (:documentation
   "Set the box of `stream'.

Note that this method manipulate the inner box directly. "))

(defmethod set-stream-box ((box coordinated-box) left right bottom top)
  (with-slots (%uv-left %uv-right %uv-bottom %uv-top) box
    (setf %uv-left   left
          %uv-right  right
          %uv-bottom bottom
          %uv-top    top)))

;; ========== stream-bounding-box ==========

(defgeneric stream-bounding-box (stream)
  (:documentation
   "Get the bounding box of `stream'.

Return values are outer edge left, right, bottom, top of the uv box. "))

(defmethod stream-bounding-box ((box coordinated-box))
  (stream-box box))

;; ========== set-stream-bounding-box ==========

(defgeneric set-stream-bounding-box (stream left right bottom top)
  (:documentation
   "Set the coordinated-box outside bounding box.

Return values are the setted coordinates lfet, right, bottom and top. "))

(defmethod set-stream-bounding-box
    ((box coordinated-box) left right bottom top)
  (set-stream-box box left right bottom top))

