(in-package :gurafu/plot)

;; ========== helper functions ==========

(defun make-linear-color-mapper (&optional (min-color *background-color*)
                                   (max-color *foreground-color*))
  "Make a color mapper function map unit value between 0 and 1 to min and max color.
Return a function. "
  (linear-color-map min-color max-color))

;; ========== 2d-grid-pane ==========

(defclass 2d-grid-pane (basic-plot-pane)
  ((%plot-data :initform () :initarg :plot-data)
   (%z-min :initform -1 :initarg :z-min)
   (%z-max :initform 1  :initarg :z-max)
   (%color-map-fn))
  (:documentation
   "Draw a 2d grid on pane. "))

(def-plot-pane-scale 2d-grid-pane :normal :log-log)

;; ========== initialize-instance ==========

(defmethod initialize-instance :after ((grid 2d-grid-pane)
                                       &key (color nil color-set-p?)
                                         (min-color *background-color*)
                                         (max-color *foreground-color*))
  (if (and color-set-p? (functionp color))
      (setf (slot-value grid '%color-map-fn) color)
      (setf (slot-value grid '%color-map-fn)
            (make-linear-color-mapper min-color max-color))))

;; ========== rescale-plot-pane ==========

(defmethod rescale-plot-pane ((grid 2d-grid-pane))
  (declare (ignore grid)))

;; ========== present ==========

(defmethod present ((grid 2d-grid-pane))
  (multiple-value-bind (%x-min %x-max %y-min %y-max)
      (xy-bounding-box grid)
    (with-slots (%plot-data %color-map-fn %z-max %z-min)
        grid
      (loop with height = (float (/ (- %y-max %y-min) (length %plot-data)))
            for row in %plot-data
            for y-max from %y-max downto %y-min by height
            do (loop with width = (float (/ (- %x-max %x-min) (length row)))
                     for gridv in row
                     for unit = (float (/ (- gridv %z-min) (- %z-max %z-min)))
                     for x-min from %x-min to %x-max by width
                     do (draw-rect grid x-min y-max (+ x-min width) (- y-max height)
                                   :color (funcall %color-map-fn unit)))))))
