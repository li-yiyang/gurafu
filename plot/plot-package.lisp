(defpackage :gurafu/plot
  (:use :cl :gurafu/core)
  (:import-from :alexandria
                :with-gensyms)
  (:export
   #:make-linear-color-mapper

   #:basic-plot-pane
   #:line-plot-pane
   #:histogram-pane
   #:2d-grid-pane
   #:2d-histogram-pane
   #:scatter-pane
   
   #:plot
   
   #:rescale-plot-pane
   #:add-plot-pane
   #:add-plot-data
   #:add-plot-decorator

   ;; widgets
   #:label
   #:legend
   #:add-plot-legend
   )
  (:documentation
   "The main plot defination for GURAFU. "))

(in-package :gurafu/plot)
