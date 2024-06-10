(in-package :gurafu/plot)

(defclass scatter-pane (basic-plot-pane)
  ((%point-style :initform :circle
                 :initarg :point-style)
   (%point-size  :initform 2
                 :initarg :point-size)
   (%color       :initform *foreground-color*
                :initarg :color))
  (:documentation
   "This is 2d scatter plot pane. "))

(def-plot-pane-scale scatter-pane :normal :log-log)

;; ========== rescale-plot-pane ==========

(defmethod rescale-plot-pane ((scatter scatter-pane))
  (with-slots (%plot-data) scatter
    (macrolet ((got (type pos init)
                 `(reduce #',type (mapcar #',pos %plot-data)
                          :initial-value ,init)))
      (multiple-value-bind (x-min x-max y-min y-max)
          (xy-bounding-box scatter)
        (set-xy-bounding-box scatter
                             (got min first x-min)
                             (got max first x-max)
                             (got min second y-min)
                             (got max second y-max))))))

;; ========== present ==========

(defmethod present ((scatter scatter-pane))
  (multiple-value-bind (%x-min %x-max %y-min %y-max)
      (xy-bounding-box scatter)
    (with-slots (%point-style %point-size %color) scatter
      (loop for (x y) in (slot-value scatter '%plot-data)

            if (and (<= %x-min x) (<= x %x-max)
                    (<= %y-min y) (<= y %y-max))
              do (draw-point scatter x y
                             :pen-width %point-size
                             :point-style %point-style
                             :color %color)))))
