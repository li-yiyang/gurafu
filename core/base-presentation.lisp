(in-package :gurafu/core)

;; ========== helper functions ==========

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +gurafu-backends+
    '(((:opticl-backend :opticl)
       gurafu/backends/opticl:opticl-backend))
    "The GURAFU backends. "))

(defmacro make-backend (type &key (colorspace :8-bit-rgb)
                               (height 100) (width 100))
  "Make a"
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
variables. "))

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

