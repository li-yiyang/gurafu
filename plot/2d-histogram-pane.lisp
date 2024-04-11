(in-package :gurafu/plot)

;; ========== helper functions ==========

(defun 2d-hist-to-plot-data (hist-data bins x-min x-max y-min y-max)
  (let ((plot-data (make-array (list bins bins)))
        (x-bin-w (float (/ (- x-max x-min) bins)))
        (y-bin-w (float (/ (- y-max y-min) bins)))
        (z-max 1))
    (loop for (x y) in hist-data
          for idx = (truncate (/ (- x x-min) x-bin-w))
          for idy = (truncate (/ (- y y-min) y-bin-w))
          if (and (<= 0 idx) (< idx bins)
                  (<= 0 idy) (< idy bins))
            do (incf (aref plot-data idy idx)))
    (values (loop for i below bins
                  collect (loop for j below bins
                                for grid-v = (aref plot-data i j)
                                do (setf z-max (max z-max grid-v))
                                collect grid-v))
            z-max)))

;; ========== 2d-histogram-pane ==========

(defclass 2d-histogram-pane (2d-grid-pane)
  ((%histogram-data :initform () :initarg :plot-data)
   (%z-min :initform 0)
   (%z-max :initform nil :initarg :z-max)
   (%plot-data)
   (%histogram-bins :initform 100 :initargs :bins)))

(defmethod initialize-instance :after ((2d-hist 2d-histogram-pane)
                                       &key)
  (multiple-value-bind (x-min x-max y-min y-max)
      (xy-bounding-box 2d-hist)
    (with-slots (%histogram-data %histogram-bins %plot-data %z-max)
        2d-hist
      (multiple-value-bind (plot-data z-max)
          (2d-hist-to-plot-data %histogram-data %histogram-bins
                                x-min x-max y-min y-max)
        (setf %plot-data plot-data
              %z-max (or %z-max z-max))))))

(defmethod set-xy-bounding-box :after ((2d-hist 2d-histogram-pane) x-min x-max y-min y-max)
  (with-slots (%histogram-data %histogram-bins %plot-data %z-max)
      2d-hist
    (multiple-value-bind (plot-data z-max)
          (2d-hist-to-plot-data %histogram-data %histogram-bins
                                x-min x-max y-min y-max)
        (setf %plot-data plot-data
              %z-max (or %z-max z-max)))))
