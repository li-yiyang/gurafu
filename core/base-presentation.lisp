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

;; ========== base-presentation ==========

(defclass base-presentation (margined-mixin)
  ((%backend :initform nil :initarg :backend))
  (:documentation
   "This is the base class for GURAFU presentation object. "))

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

(defgeneric present (obj &optional media)
  (:documentation
   "The generic function about how the `obj' is presented. "))

