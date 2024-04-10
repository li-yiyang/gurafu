(in-package :gurafu/plot)

;; ========== basic-plot-pane ==========

(defclass basic-plot-pane (xy-box-present)
  ((%plot-data :initform nil :initarg :plot-data))
  (:documentation
   "This is the base class for showing a plot.

The `plot-pane' is a place where `%plot-data' would be shown.
The `%plot-data' should be abitrary data sets which should be
used to draw by `present'.

All plot-pane should be class inherited from `basic-plot-pane'.
The plot-pane should hook it's own `present' methods. "))

;; ========== rescale-plot-pane ==========

(defgeneric rescale-plot-pane (plot-pane)
  (:documentation
   "Rescale the plot-pane automatically.
Return values are rescaled plot-pane xy-bounding-box. "))
