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
   #:+gray+
   #:*foreground-color*
   #:*background-color*
   #:+绛+
   #:+赤+
   #:+朱+
   #:+丹+
   #:+红+
   #:+妃红+
   #:+品红+
   #:+海棠+
   #:+胭脂+
   #:+绯+
   #:+赭+
   #:+茜+
   #:+殷红+
   #:+桃红+
   
   ;; coordinated-box
   #:coordinated-box
   #:stream-box
   #:set-stream-box
   #:stream-bounding-box
   #:set-stream-bounding-box

   ;; margined-mixin
   #:margined-mixin
   #:stream-margins
   #:set-stream-margins

   ;; base-presentation
   #:+gurafu-backends+
   #:make-backend
   #:with-present-to-file
   
   #:*default-backends*
   
   #:base-presentation
   #:present
   #:draw-point
   #:draw-text
   #:draw-text-size
   #:draw-triangle
   #:draw-rect
   #:draw-circle
   #:draw-line

   ;; xy-box-present
   #:xy-box-mixin
   #:xy-box-present
   #:with-xy-to-uv
   #:with-uv-to-xy
   #:xy-bounding-box
   #:set-xy-bounding-box

   ;; layout-presentation
   #:stack-layout-presentation
   #:vertical-layout-presentation
   #:horizontal-layout-presentation
   #:get-component
   #:add-component
   #:loop-components

   ;; define-presentation
   #:define-presentation

   ;; plot
   #:basic-plot-pane
   #:line-plot-pane
   #:plot
   #:add-plot-pane
   #:add-plot-data
   #:get-plot-pane
   ))

(in-package :gurafu/core)
