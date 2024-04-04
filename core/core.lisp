(defpackage :gurafu/core
  (:use
   :cl
   :gurafu/protocol
   :gurafu/backends/opticl)
  (:import-from
   :alexandria
   :maphash-values
   :with-gensyms))

(in-package :gurafu/core)
