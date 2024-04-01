(in-package :gurafu/protocol)

(defclass base-protocol ()
  ((width  :initarg :width  :initform 100 :accessor stream-width!)
   (height :initarg :height :initform 100 :accessor stream-height!))
  (:documentation
   "The basic protocol class. "))
