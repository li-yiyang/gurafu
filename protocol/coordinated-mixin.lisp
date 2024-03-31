(in-package :gurafu/protocol)

;; ========== coordinated-mixin ==========

(defclass coordinated-mixin ()
  ((uv-left   :initform 0)
   (uv-right  :initform 100)
   (uv-bottom :initform 100)
   (uv-top    :initform 0)
   (xy-left   :initform 0.0)
   (xy-right  :initform 1.0)
   (xy-bottom :initform 0.0)
   (xy-top    :initform 1.0))
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

;; ========== stream-inner-width! ==========

(defrequired stream-inner-width! (stream)
  (:documentation
   "Get the inner width of `stream'. "))

(defmethod stream-inner-width! ((stream coordinated-mixin))
  (with-slots (uv-left uv-right) stream
    (- uv-right uv-left)))

;; ========== set-stream-inner-width! ==========

(defrequired set-stream-inner-width! (stream width &optional left)
  (:documentation
   "Set the inner width of `stream'. "))

(defmethod set-stream-inner-width! ((stream coordinated-mixin) width
                                    &optional (left 0 left-set?))
  (with-slots (uv-left uv-right) stream
    (when left-set?
      (setf uv-left left))
    (setf uv-right (+ uv-left width))))

;; ========== stream-inner-height! ==========

(defrequired stream-inner-height! (stream)
  (:documentation
   "Get the inner height of `stream'. "))

(defmethod stream-inner-height! ((stream coordinated-mixin))
  (with-slots (uv-top uv-bottom) stream
    (- uv-bottom uv-top)))

;; ========== set-stream-inner-height! ==========

(defrequired set-stream-inner-height! (stream height &optional top)
  (:documentation
   "Set the inner height of `stream'. "))

(defmethod set-stream-inner-height! ((stream coordinated-mixin) height
                                     &optional (top 0 top-set?))
  (with-slots (uv-top uv-bottom) stream
    (when top-set?
      (setf uv-top top))
    (setf uv-bottom (+ uv-top height))))

;; ========== stream-width! ==========

(defrequired stream-width! (stream)
  (:documentation
   "The out uv width of `stream'. "))

(defmethod stream-width! ((stream coordinated-mixin))
  (stream-inner-width! stream))

;; ========== set-stream-width! ==========

(defrequired set-stream-width! (stream width &optional left)
  (:documentation
   "Set the out uv width of `stream'. "))

(defmethod set-stream-width! ((stream coordinated-mixin) width
                              &optional (left 0 left-set?))
  (if left-set?
      (set-stream-inner-width! stream width left)
      (set-stream-inner-width! stream width)))

;; ========== stream-height! ==========

(defrequired stream-height! (stream)
  (:documentation
   "The out uv height of `stream'. "))

(defmethod stream-height! ((stream coordinated-mixin))
  (stream-inner-height! stream))

;; ========== set-stream-height! ==========

(defrequired set-stream-height! (stream height &optional top)
  (:documentation
   "The out uv height of `stream'. "))

(defmethod set-stream-height! ((stream coordinated-mixin) height
                           &optional (top 0 top-set?))
  (if top-set?
      (set-stream-inner-height! stream height top)
      (set-stream-inner-height! stream height)))

;; ========== stream-inner-pos! ==========

(defrequired stream-inner-pos! (stream)
  (:documentation
   "Get the inner position of stream. "))

(defmethod stream-inner-pos! ((stream coordinated-mixin))
  (with-slots (uv-left uv-top) stream
    (values uv-left uv-top)))

;; ========== set-stream-inner-pos! ==========

(defrequired set-stream-inner-pos! (stream left top)
  (:documentation
   "Set the inner position of `stream'. "))

(defmethod set-stream-inner-pos! ((stream coordinated-mixin) left top)
  (with-slots (uv-left uv-right uv-top uv-bottom) stream
    (let ((w-shift (- left uv-left))
          (h-shift (- top  uv-top)))
      (setf uv-left   left
            uv-top    top
            uv-right  (+ uv-right w-shift)
            uv-bottom (+ uv-bottom h-shift)))))

;; ========== stream-pos! ==========

(defrequired stream-pos! (stream)
  (:documentation
   "The absolute position of `stream'.
Return `top', `bottom'. "))

(defmethod stream-pos! ((stream coordinated-mixin))
  (stream-inner-pos! stream))

;; ========== set-stream-pos! ==========

(defrequired set-stream-pos! (stream u v)
  (:documentation
   "Set the position of `strem'. "))

(defmethod set-stream-pos! ((stream coordinated-mixin) left top)
  (set-stream-inner-pos! stream left top))

;; ========== xy-to-stream-uv! ==========

(defrequired xy-to-stream-uv! (stream x y)
  (:documentation
   "Transform `x', `y' on `stream' to digitial `u', `v'.
Return values of `u' and `v'. "))

(defmethod xy-to-stream-uv! ((stream coordinated-mixin) x y)
  (with-slots (uv-left uv-right uv-top uv-bottom
               xy-left xy-right xy-top xy-bottom)
      stream
    (values
     (truncate (+ uv-left   (* (- x xy-left)
                               (/ (- uv-right uv-left)
                                  (- xy-right xy-left)))))
     (truncate (- uv-bottom (* (- y xy-bottom)
                               (/ (- uv-bottom uv-top)
                                  (- xy-top xy-bottom))))))))

;; ========== xy-to-stream-uv-transformer! ==========

(defrequired xy-to-stream-uv-transformer! (stream)
  (:documentation
   "Transformer from `x', `y' on `stream' to digitial `u', `v'.
Return a function accepting `x' and `y' for transformation. "))

(defmethod xy-to-stream-uv-transformer! ((stream coordinated-mixin))
  (with-slots (uv-left uv-right uv-top uv-bottom
               xy-left xy-right xy-top xy-bottom)
      stream
    (let* ((w-scale (/ (- uv-right uv-left) (- xy-right xy-left)))
           (h-scale (/ (- uv-top uv-bottom) (- xy-top xy-bottom)))
           (u0 (+ uv-left   (* xy-left   w-scale)))
           (v0 (+ uv-bottom (* xy-bottom h-scale))))
      (lambda (x y)
        (values (truncate (+ u0 (* x w-scale)))
                (truncate (+ v0 (* y h-scale))))))))

;; ========== with-xy-to-stream ==========
(defmacro with-xy-to-stream-uv (stream bindings &body body)
  ""
  
  (alexandria:with-gensyms (trans)
    `(let ((,trans (xy-to-stream-uv-transformer! ,stream)))
       ,(loop for code = `(progn ,@body) then
              `(multiple-value-bind ,xys (funcall ,trans ,@uvs)
                 ,code)
              for (xys uvs) in (reverse bindings)
              finally (return code)))))

;; ========== uv-to-stream-xy! ==========

(defrequired uv-to-stream-xy! (stream u v)
  (:documentation
   "Inverse transform `u', `v' on `stream' to `x', `y'.
Return values of `x' and `y'. "))

;; ========== uv-to-stream-xy-transformer! ==========

(defrequired uv-to-stream-xy-transformer! (stream)
  (:documentation
   ""))
