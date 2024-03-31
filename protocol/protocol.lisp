(defpackage :gurafu/protocol
  (:use :cl)
  (:export
   ;; output-protocol
   #:output-protocol
   #:output-p?
   #:init-output!
   #:output!
   #:draw-point!
   #:draw-line!
   #:draw-rect!
   #:draw-tringle!
   #:draw-circle!
   #:draw-text!

   ;; coordinated-mixin
   #:stream-width!
   #:set-stream-width!
   #:stream-height!
   #:set-stream-height!
   #:stream-inner-width!
   #:set-stream-inner-width!
   #:stream-inner-height!
   #:set-stream-inner-height!
   #:stream-pos!
   #:set-stream-pos!
   #:stream-inner-pos!
   #:set-stream-inner-pos!
   #:xy-to-stream-uv!
   #:xy-to-stream-uv-transformer!
   #:uv-to-stream-xy!
   #:uv-to-stream-xy-transformer!

   #:with-xy-to-stream-uv

   ;; margined-mixin
   #:stream-margins!
   #:set-stream-margins!

   ;; colored-mixin
   #:+white+
   #:+black+
   #:+colorful-colorspace+
   #:+grayful-colorspace+
   #:*foreground-color*
   #:*background-color*
   #:colorspace
   #:colorful?
   #:real-colorspace-name!
   #:rgb-color!
   )
  (:documentation
   "The GURAFU backend should use `gurafu/protocol' as foundamental
plotting/interacting instructions.

Naming convention:
+ the method postfix with `!' stands for the low level instructions"))

(in-package :gurafu/protocol)
