(in-package :gurafu/protocol)

;; ========== default output parameters ==========

(defparameter *pen-width* 1
  "Default width for drawing. ")

(defparameter *line-style* :solid
  "Style for drawing line.

NOTE: NOT IMPLEMENTED YET. ")

(defparameter *fill?* t
  "None-nil for not fill for shapes, otherwise fill. ")

(defparameter *point-style* :point
  "Style to draw point. ")

(defparameter *font-color* *foreground-color*
  "Font color. ")

(defparameter *font-size* 16
  "Font size. ")

(defparameter *char-forward* '(1.0 0.0)
  "Text forward direction (in screen uv cooridnary) in `u' and `v'.

For example:

   ------>  (1.0 0.0)  \  (1.0 1.0)
   t e x t            t \
                       x \
                        t V
")

(defparameter *char-spacing* 1.2
  "The ratio of two character distance to char width. ")

(defparameter *line-forward* '(0.0 1.0)
  "Direction to break a line character.
It should not parallel with `*text-path*'.

For example:
           (0.0 1.0)           (2.0 1.0)
  | line 1            \ line 1
  V line 2             \  line 2
")

(defparameter *line-spacing* 1.0
  "The ratio of two line distacne to font-size. ")

(defparameter *text-align* :normal
  "Text align method.

Could be:
+ :normal, :left, left-top, top-left
+ :center, centered
+ :horizontal-center, :top-center
+ :veritcal-center, :left-vertical-center, :vertical-center-left
+ :right, right-top, :top-right
+ :bottom :left-bottom
+ :right-bottom
+ :right-center :right-vertical-center

these are described as their names indicated. ")


