(in-package :gurafu/plot)

;; ========== basic-plot-pane ==========

(defclass basic-plot-pane (box-present)
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

(defgeneric to-scale (plot-pane scale)
  (:documentation
   "Change the `plot-pane' to possible scale: `:normal', `:log-log'.
Which should be called when add to plot-panes. "))

(defmacro def-plot-pane-scale (plot-pane-class &rest scales)
  "Quickly set the plot pane."
  (flet ((name (scale)
           (ecase scale
             (:normal
              (intern (format nil "NORMAL-~@:(~a~)" plot-pane-class)))
             (:log-log
              (intern (format nil "LOG-LOG-~@:(~a~)" plot-pane-class)))
             (:log-y
              (intern (format nil "LOG-Y-~@:(~a~)" plot-pane-class)))
             (:log-x
              (intern (format nil "LOG-X-~@:(~a~)" plot-pane-class))))))
    (let ((classes (loop for scale in scales
                         collect (list scale (name scale)))))
      `(progn
         ,@(loop for (scale name) in classes
                 for mixin-name = (ecase scale
                                    (:normal  'xy-box-mixin)
                                    (:log-log 'log-log-xy-box-mixin)
                                    (:log-y   'y-log-xy-box-mixin)
                                    (:log-x   'x-log-xy-box-mixin))
                 collect `(defclass ,name (,plot-pane-class ,mixin-name) ()))
         (defmethod to-scale ((plot ,plot-pane-class) scale)
           (ecase scale
             ,@(loop for (scale name) in classes
                     collect `(,scale
                               (change-class plot ',name)
                               (reinitialize-instance plot)))))))))
