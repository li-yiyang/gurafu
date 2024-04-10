(defpackage :gurafu/plot
  (:use :cl :gurafu/core)
  (:export
   #:basic-plot-pane
   #:line-plot-pane
   #:histogram-pane
   
   #:plot
   
   #:rescale-plot-pane)
  (:documentation
   "The main plot defination for GURAFU. "))

(in-package :gurafu/plot)
