(in-package :gurafu/protocol)

;; ========== margined-mixin ==========

(defclass margined-mixin (coordinated-mixin)
  ((margin-left   :initform 0 :initarg :margin-left)
   (margin-right  :initform 0 :initarg :margin-right)
   (margin-bottom :initform 0 :initarg :margin-bottom)
   (margin-top    :initform 0 :initarg :margin-top))
  (:documentation
   "This class defines how the margined is used.

     *-----------------------------------------*-> u
     |            y^ margin-top                |
     |             +----------+                |
     |             |          |                |
     | margin-left |          | margin-right   | stream-height!
     |             |          |                |
     |             +----------+-> x            |
     |             margin-bottom               |
     *-----------------------------------------*
     V v            stream-width!
"))

;; ========== stream-margins! ==========

(defrequired stream-margins! (stream)
  (:documentation
   "Get the margin sizes of `stream'.
Return margin left, right, top, bottom. "))

(defmethod stream-margins! ((stream margined-mixin))
  (with-slots (margin-left margin-right margin-bottom margin-top)
      stream
    (values margin-left margin-right margin-bottom margin-top)))

;; ========== set-stream-margins! ==========

(defrequired set-stream-margins! (stream left right top bottom)
  (:documentation
   "Set the margin sizes of `stream'. "))

(defmethod set-stream-margins! ((stream margined-mixin)
                                left right top bottom)
  (with-slots (margin-left margin-right margin-bottom margin-top)
      stream
    (setf margin-left   left
          margin-right  right
          margin-bottom bottom
          margin-top    top)))

;; ========== stream-width! ==========

(defmethod stream-width! :around ((stream margined-mixin))
  (let ((inner-width (call-next-method)))
    (with-slots (margin-left margin-right) stream
      (+ inner-width margin-left margin-right))))

;; ========== stream-height! ==========

(defmethod stream-height! :around ((stream margined-mixin))
  (let ((inner-height (call-next-method)))
    (with-slots (margin-top margin-bottom) stream
      (+ inner-height margin-top margin-bottom))))

;; ========== set-stream-width! ==========

(defmethod set-stream-width! :around ((stream margined-mixin) width
                                      &optional (left 0 left-set?))
  (with-slots (margin-left margin-right) stream
    (let ((inner-width (- width margin-left margin-right)))
      (if (> inner-width 0)
          (if left-set?
              (call-next-method stream inner-width left)
              (call-next-method stream inner-width))
          (error
           (format nil
                   "Cannot set width for inner width ~a is too small"
                   inner-width))))))

;; ========== set-stream-height! ==========

(defmethod set-stream-height! :around ((stream margined-mixin) height
                                       &optional (top 0 top-set?))
  (with-slots (margin-top margin-bottom) stream
    (let ((inner-height (- height margin-top margin-bottom)))
      (if (> inner-height 0)
          (if top-set?
              (call-next-method stream inner-height top)
              (call-next-method stream inner-height))
          (error
           (format nil
                   "Cannot set height for inner height ~a is too small"
                   inner-height))))))
