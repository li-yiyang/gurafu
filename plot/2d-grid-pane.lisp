(in-package :gurafu/plot)

;; ========== helper functions ==========

(defun make-linear-color-mapper (&optional (min-color *background-color*)
                                   (max-color *foreground-color*))
  "Make a color mapper function map unit value between 0 and 1 to min and max color.
Return a function. "
  (lambda (w)
    (let ((w (min 1.0 (max w 0.0))))
      (mapcar (lambda (min max) (+ (* w min) (* (- 1 w) max)))
              min-color max-color))))

;; ========== 2d-grid-pane ==========

(defclass 2d-grid-pane (basic-plot-pane)
  ((%plot-data :initform () :initarg :plot-data)
   (%z-min :initform -1 :initarg :z-min)
   (%z-max :initform 1  :initarg :z-max)
   (%color-map-fn))
  (:documentation
   "Draw a 2d grid on pane. "))

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
    (with-slots (%plot-data %color-map-fn)
        grid
      (loop with height = (float (/ (- %y-max %y-min) (length %plot-data)))
            for row in %plot-data
            for y-max from %y-max downto %y-min by height
            do (loop with width = (float (/ (- %x-max %x-min) (length row)))
                     for gridv in row
                     for x-min from %x-min to %x-max by width
                     do (draw-rect grid x-min y-max (+ x-min width) (- y-max height)
                                   :color (funcall %color-map-fn gridv)))))))

;; ========== test ==========

(with-present-to-file (plot "~/Buff/test.png" plot
                            :margin 20)
  (add-plot-data plot 'grid
                 (loop for y from -10 upto 10 by 0.5
                       collect (loop for x from -10 upto 10 by 0.5
                                     collect (sin (sqrt (+ (* x x) (* y y))))))
                 :pane-type '2d-grid-pane
                 :color (make-linear-color-mapper +white+ +桃红+))
  (set-xy-bounding-box plot -10.0 10.0 -10.0 10.0))
