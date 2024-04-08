(defpackage :gurafu/core
  (:use
   :cl
   :gurafu/protocol
   :gurafu/backends/opticl)
  (:import-from
   :alexandria
   :maphash-values
   :with-gensyms)
  (:export
   ;; colors
   #:+white+
   #:+black+
   #:+red+
   #:+green+
   #:+blue+
   #:+yellow+
   #:*foreground-color*
   #:*background-color*
   
   ;; coordinated-box
   #:stream-box
   #:set-stream-box
   #:stream-bounding-box
   #:set-stream-bounding-box

   ;; margined-mixin
   #:stream-margins
   #:set-stream-margins

   ;; base-presentation
   #:+gurafu-backends+
   #:make-backend
   
   #:*default-backends*
   
   #:base-presentation
   #:present
   #:draw-point
   #:draw-text
   #:draw-triangle
   #:draw-rect
   #:draw-circle
   #:draw-line   

   ;; layout-presentation
   #:stack-layout-presentation
   #:vertical-layout-presentation
   #:horizontal-layout-presentation

   ;; define-presentation
   #:define-presentation
   ))

(in-package :gurafu/core)
