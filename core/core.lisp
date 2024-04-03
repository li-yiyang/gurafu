(defpackage :gurafu/core
  (:use
   :cl
   :gurafu/protocol
   :gurafu/backends/opticl)
  (:import-from
   :alexandria
   :maphash-values))

(in-package :gurafu/core)
