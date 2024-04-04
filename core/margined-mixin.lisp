(in-package :gurafu/core)

;; ========== margined-mixin ==========
;; The `margined-mixin' should work on `coordinated-box'
;; object, which provide the margined feature for
;; coordinate-box.

(defclass margined-mixin ()
  ((%margin-left   :initform 0)
   (%margin-right  :initform 0)
   (%margin-bottom :initform 0)
   (%margin-top    :initform 0))
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

;; ========== initialize-instance ==========

(defmethod initialize-instance :after
    ((stream margined-mixin) &key (margin 0))
  (cond ((numberp margin)               ; evenly distributed margin 
         (set-stream-margins stream margin margin margin margin))
        ((listp margin)                 ; margin by specify four edges
         (set-stream-margins stream
                             (or (first  margin) 0)
                             (or (second margin) 0)
                             (or (third  margin) 0)
                             (or (fourth margin) 0)))
        (t (error (format nil "Malformed margin of ~a. " margin)))))

;; ========== stream-margins ==========

(defgeneric stream-margins (stream)
  (:documentation
   "Get the `stream' margins.
Return values are left, right, bottom, top of stream margin. "))

(defmethod stream-margins ((stream margined-mixin))
  (with-slots (%margin-left %margin-right %margin-bottom %margin-top)
      stream
    (values %margin-left %margin-right %margin-bottom %margin-top)))

;; ========== set-stream-margins ==========

(defgeneric set-stream-margins (stream left right bottom top)
  (:documentation
   "Set the `stream' margins. "))

(defmethod set-stream-margins ((stream margined-mixin) left right bottom top)
  (with-slots (%margin-left %margin-right %margin-bottom %margin-top) stream
    (setf %margin-left   left
          %margin-right  right
          %margin-bottom bottom
          %margin-top    top)))

;; ========== stream-bounding-box ==========

(defmethod stream-bounding-box :around ((stream margined-mixin))
  (with-slots (%margin-left %margin-right %margin-bottom %margin-top)
      stream
    (multiple-value-bind (left right bottom top)
        (call-next-method stream)
      (values (- left   %margin-left)
              (+ right  %margin-right)
              (+ bottom %margin-bottom)
              (+ top    %margin-top)))))

;; ========== set-stream-bounding-box ==========

(defmethod set-stream-bounding-box :around
    ((stream margined-mixin) left right bottom top)
  (with-slots (%margin-left %margin-right %margin-bottom %margin-top)
      stream
    (if (and (> (- right left) (+ %margin-left   %margin-right))
             (> (- bottom top) (+ %margin-bottom %margin-top)))
        (call-next-method stream
                          (+ left   %margin-left)
                          (- right  %margin-right)
                          (- bottom %margin-bottom)
                          (+ top    %margin-top))
        (error (format nil "No enough space for margined stream box. ")))))
