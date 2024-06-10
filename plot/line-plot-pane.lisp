(in-package :gurafu/plot)

;; ========== line-plot-pane ==========

(defclass line-plot-pane (basic-plot-pane)
  ((%line-style  :initform :solid
                 :initarg :line-style)
   (%line-color  :initform *foreground-color*
                 :initarg :color)
   (%line-width  :initform 2
                 :initarg  :line-width)
   (%point-style :initform nil
                 :initarg  :point-style)
   (%point-size  :initform 2
                 :initarg  :point-size)   
   (%point-color :initform *foreground-color*
                 :initarg  :point-color))
  (:documentation
   "Draw a line for `%plot-data'. "))

(def-plot-pane-scale line-plot-pane :normal :log-log :log-y :log-x)

;; ========== rescale-plot-pane ==========

(defmethod rescale-plot-pane ((plot line-plot-pane))
  (with-slots (%plot-data) plot
    (macrolet ((got (type pos)
                 `(reduce #',type (mapcar #',pos %plot-data))))
      (set-xy-bounding-box
       plot (got min first) (got max first)
       (got min second) (got max second)))))

;; ========== present ==========

(defmethod present ((plot line-plot-pane))
  (multiple-value-bind (x-min x-max y-min y-max)
      (xy-bounding-box plot)
    (with-slots (%line-style %line-color %line-width
                 %point-style %point-size %point-color
                 %plot-data)
        plot
      (flet ((constrain (a x b)
               (if (< a b)
                   (max a (min x b))
                   (max b (min x a)))))
        (loop for ((x10 y10) (x20 y20)) on (slot-value plot '%plot-data)
              while (and x20 y20)          

              for x1 = (constrain x-min x10 x-max)
              for y1 = (constrain y-min y10 y-max)
              for x2 = (constrain x-min x20 x-max)
              for y2 = (constrain y-min y20 y-max)
              
              do (draw-line plot x1 y1 x2 y2
                            :line-style %line-style
                            :color      %line-color
                            :pen-width  %line-width)
                 
              if %point-style
                do (draw-point plot x1 y1
                               :point-style %point-style
                               :pen-width   %point-size
                               :color       %point-color))))))
