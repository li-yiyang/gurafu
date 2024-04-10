(defsystem #:gurafu
  :author ("凉凉")
  :version "0"
  :description "This is a package that deals with ploting."
  :depends-on ()
  :components ((:file "package")))

(defsystem #:gurafu/protocol
  :author ("凉凉")
  :version "0"
  :description
  "This part defines how the GURAFU backend protocol
should be implemented. All the GURAFU backend should
follow the behavior defined in `gurafu/protocol'. "
  :depends-on (:alexandria)
  :pathname "protocol"
  :serial t
  :components ((:file "protocol")
               (:file "protocol-macro")
               (:file "base-protocol")
               (:file "colored-mixin")
               (:file "output-protocol")))

(defsystem #:gurafu/core
  :author ("凉凉")
  :version "0"
  :description "This is the core GURAFU package. "
  :depends-on (:alexandria
               :gurafu/protocol
               :gurafu/backends/opticl)
  :pathname "core"
  :serial t
  :components ((:file "core")
               (:file "colors")
               (:file "coordinated-box")
               (:file "margined-mixin")
               (:file "base-presentation")
               (:file "xy-box-present")               
               (:file "layout-presentation")
               (:file "define-presentation")
               (:file "plot")))

(defsystem #:gurafu/backends/opticl
  :author ("凉凉")
  :version "0"
  :description "The GURAFU output backend with opticl."
  :depends-on (:gurafu/protocol
               :opticl               
               :cl-bdf)
  :pathname "backends/opticl"
  :serial t
  :components ((:file "opticl")               
               (:file "opticl-backend")))
