(in-package :gurafu/plot)

;; ========== helper functions ==========

(defun hist-to-plot-data (hist-data bins min max)
  "Trun `hist-data' (a list) into `bins' histogram with range `min' and `max'.

Return values are histogram list with element: (mid count left right). 

Please note that each bin is data within [left right), so the histogram
result may loss the `max' point of `hist-data'."
  (let ((plot-data (make-array (list bins)))
        (bin-width (float (/ (- max min) bins))))
    (loop for dat in hist-data
          for idx = (truncate (/ (- dat min) bin-width))
          if (and (< idx bins) (>= idx 0))
            do (incf (aref plot-data idx)))
    (loop for idx below bins
          for left = (+ min (* bin-width idx))
          for right = (+ min (* bin-width (1+ idx)))
          for mid = (/ (+ left right) 2.0)
          collect (list mid (aref plot-data idx) left right))))

;; ========== histogram-pane ==========

(defclass histogram-pane (basic-plot-pane)
  ((%histogram-data :initform () :initarg :plot-data)
   (%plot-data)
   (%histogram-bins :initform 100 :initarg :bins)
   (%histogram-color :initform *foreground-color* :initarg :color)
   (%histogram-line-width :initform 1 :initarg :line-width))
  (:documentation
   "Draw the histogram plot. "))

(def-plot-pane-scale histogram-pane :normal :log-log)

;; ========== rescale-plot-pane ==========

(defmethod rescale-plot-pane ((hist histogram-pane))
  (multiple-value-bind (x-min x-max y-min y-max)
      (xy-bounding-box hist)    
    (with-slots (%plot-data) hist
      (let ((x-min (min x-min (apply #'min (mapcar #'first  %plot-data))))
            (x-max (max x-max (apply #'max (mapcar #'first  %plot-data))))
            (y-min (min y-min (apply #'min (mapcar #'second %plot-data))))
            (y-max (max y-max (apply #'max (mapcar #'second %plot-data)))))
        (set-xy-bounding-box hist x-min x-max y-min y-max)))))

;; ========== set-xy-bounding-box ==========

(defmethod set-xy-bounding-box :after
    ((hist histogram-pane) x-min x-max y-min y-max)
  (with-slots (%histogram-data %histogram-bins %plot-data) hist
    (setf %plot-data (hist-to-plot-data %histogram-data %histogram-bins
                                        x-min x-max))))

;; ========== present ==========

(defmethod present ((hist histogram-pane))
  (multiple-value-bind (x-min x-max y-min y-max)
      (xy-bounding-box hist)
    (with-slots (%histogram-color %histogram-line-width) hist
      (flet ((constrain (a x b)
               (if (< a b)
                   (max a (min x b))
                   (max b (min x a)))))
        (loop for (mid0 c0 l0 r0) in (slot-value hist '%plot-data)

              for c = (constrain y-min c0 y-max)
              for l = (constrain x-min l0 x-max)
              for r = (constrain x-min r0 x-max)

              do (draw-rect hist l c r 0
                            :fill? t
                            :fill-color %histogram-color
                            :line-width %histogram-line-width))))))
