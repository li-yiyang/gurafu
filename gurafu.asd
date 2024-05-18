(defsystem #:gurafu
  :author ("凉凉")
  :version "0"
  :license "MIT"
  :description "This is a package that deals with ploting."
  :depends-on (:gurafu/plot :gurafu/core :trivial-indent)
  :components ((:file "package")
               (:file "indent")))

(defsystem #:gurafu/plot
  :author ("凉凉")
  :version "0"
  :license "MIT"
  :description "This is a package that draw the plot on GURAFU. "
  :pathname "plot"
  :depends-on (:gurafu/core)
  :components ((:file "plot-package")
               ;; plot-panes
               (:file "basic-plot-pane")
               (:file "line-plot-pane")
               (:file "histogram-pane")
               (:file "2d-grid-pane")
               (:file "2d-histogram-pane")
               (:file "scatter-pane")
               ;; widgets
               (:file "label")
               (:file "plot")))

(defsystem #:gurafu/protocol
  :author ("凉凉")
  :version "0"
  :license "MIT"
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
               (:file "closures")
               (:file "output-protocol")))

(defsystem #:gurafu/core
  :author ("凉凉")
  :version "0"
  :license "MIT"
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
               (:file "define-presentation")))

(defsystem #:gurafu/backends/opticl
  :author ("凉凉")
  :version "0"
  :license "MIT"
  :description "The GURAFU output backend with opticl."
  :depends-on (:gurafu/protocol
               :opticl               
               :cl-bdf)
  :pathname "backends/opticl"
  :serial t
  :components ((:file "opticl")               
               (:file "opticl-backend")))
