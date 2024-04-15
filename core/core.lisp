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
   #:lab-to-rgb-color
   #:xyz-to-rgb-color
   #:linear-color-map
   #:+white+
   #:+black+
   #:+red+
   #:+green+
   #:+blue+
   #:+yellow+
   #:+gray+
   #:*foreground-color*
   #:*background-color*
   #:+大红+
   #:+莲红+
   #:+桃红+
   #:+银红+
   #:+水红+
   #:+木红+
   #:+鹅黄+
   #:+紫+
   #:+天青+
   #:+葡萄青+
   #:+蛋青+
   #:+翠蓝+
   #:+天蓝+
   #:+月白+
   #:+草白+
   #:+毛青+   
   #:+大红官绿+
   #:+豆绿+
   #:+油绿+
   #:+藕色+
   #:+茶褐+
   #:+包头青+
   
   ;; coordinated-box
   #:coordinated-box
   #:stream-box
   #:stream-box-width
   #:stream-box-height
   #:set-stream-box
   #:stream-bounding-box
   #:stream-bounding-box-width
   #:stream-bounding-box-height
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
   ))

(in-package :gurafu/core)
