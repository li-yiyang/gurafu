(defpackage :gurafu
  (:use :cl :gurafu/core :gurafu/plot)
  (:export
   ;; colors
   #:+white+
   #:+black+
   #:+red+
   #:+blue+
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

   ;; draw functions
   #:draw-point
   #:draw-text
   #:draw-text-size
   #:draw-triangle
   #:draw-rect
   #:draw-circle
   #:draw-line

   #:stream-box
   #:set-stream-box
   #:stream-bounding-box
   #:set-stream-bounding-box
   
   ;; presentation
   #:with-present-to-file
   #:define-presentation
   #:present

   ;; predefined presentation
   #:base-presentation
   #:xy-box-present

   #:stack-layout-presentation
   #:vertical-layout-presentation
   #:horizontal-layout-presentation

   #:get-component
   #:add-component
   #:loop-components

   ;; plot
   #:make-linear-color-mapper

   #:basic-plot-pane
   #:line-plot-pane
   #:histogram-pane
   #:2d-grid-pane
   #:2d-histogram-pane
   
   #:plot
   
   #:rescale-plot-pane
   #:add-plot-pane
   #:add-plot-data
   ))
