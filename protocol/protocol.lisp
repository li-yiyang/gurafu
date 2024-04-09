(defpackage :gurafu/protocol
  (:use :cl)
  (:export
   ;; base-protocol
   #:base-protocol
   #:stream-width!
   #:stream-height!

   ;; output-protocol
   #:output-protocol
   #:output-p?
   #:output!
   #:draw-point!
   #:draw-line!
   #:draw-rect!
   #:draw-tringle!
   #:draw-circle!
   #:draw-text!
   #:draw-text-size!

   ;; colored-mixin
   #:+white+
   #:+black+
   #:+red+
   #:+green+
   #:+blue+
   #:+yellow+
   #:+gray+
   #:*foreground-color*
   #:*background-color*

   #:+colorful-colorspace+
   #:+grayful-colorspace+
   #:colorspace!
   #:colorful?
   #:real-colorspace-name!
   #:rgb-color!
   )
  (:documentation
   "The GURAFU backend should use `gurafu/protocol' as foundamental
plotting/interacting instructions."))

(in-package :gurafu/protocol)
