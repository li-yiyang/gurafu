(defpackage :gurafu/backends/opticl
  (:use :cl :opticl :gurafu/protocol :cl-bdf)
  (:import-from
   :alexandria
   :make-keyword)
  (:export
   #:+opticl-support-format+   
   #:opticl-backend)
  (:documentation
   "This is GURAFU backend using opticl. "))

(in-package :gurafu/backends/opticl)
