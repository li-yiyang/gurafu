(defpackage :gurafu
  (:use :cl :gurafu/core :gurafu/plot :trivial-indent)
  (:export
   ;; closure
   #:*pen-width*
   #:*line-style*
   #:*fill?*
   #:*color*
   #:*point-style*
   #:*font-color*
   #:*text-align*
   #:*char-forward*
   #:*line-forward*
   #:*char-spacing*
   #:*line-spacing*
   #:*font-size*
   #:*font-name*

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

   ;; coordinates
   #:stream-box
   #:stream-box-width
   #:stream-box-height
   #:set-stream-box
   #:stream-bounding-box
   #:stream-bounding-box-width
   #:stream-bounding-box-height
   #:set-stream-bounding-box

   ;; xy-bounding-box
   #:*x-min*
   #:*x-max*
   #:*y-min*
   #:*y-max*
   #:set-xy-bounding-box

   ;; presentation
   #:with-present-to-file
   #:with-present
   #:define-presentation
   #:present

   ;; predefined presentation
   #:base-presentation
   #:xy-box-present

   #:stack-layout-presentation
   #:vertical-layout-presentation
   #:horizontal-layout-presentation
   #:vertical-flow-layout-presentation
   #:horizontal-flow-layout-presentation

   #:get-component
   #:add-component
   #:loop-components

   ;; plot
   #:make-linear-color-mapper

   #:label
   #:legend

   #:basic-plot-pane
   #:line-plot-pane
   #:histogram-pane
   #:unified-histogram-pane
   #:2d-grid-pane
   #:2d-histogram-pane
   #:scatter-pane

   #:plot

   #:rescale-plot-pane
   #:add-plot-pane
   #:add-plot-data
   #:add-plot-decorator
   #:add-plot-legend
   ))
