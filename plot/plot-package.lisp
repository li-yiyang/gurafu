(defpackage :gurafu/plot
  (:use :cl :gurafu/core)
  (:export
   #:make-linear-color-mapper

   #:basic-plot-pane
   #:line-plot-pane
   #:histogram-pane
   #:2d-grid-pane
   #:2d-histogram-pane
   
   #:plot
   
   #:rescale-plot-pane)
  (:documentation
   "The main plot defination for GURAFU. "))

(in-package :gurafu/plot)
