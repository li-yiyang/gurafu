(in-package :gurafu/plot)

;; ========== unified-histogram-pane ==========

(defclass unified-histogram-pane (histogram-pane) ()
  (:documentation
   "Draw the histogram plot but unified. "))

(def-plot-pane-scale unified-histogram-pane :normal)

;; ========== present ==========

(defmethod present ((hist unified-histogram-pane))
  (multiple-value-bind (x-min x-max y-min y-max)
      (xy-bounding-box hist)
    (with-slots (%histogram-color %histogram-line-width
		 %histogram-style %histogram-point-style
		 %histogram-pen-width)
	hist
      (flet ((constrain (a x b)
               (if (< a b)
                   (max a (min x b))
                   (max b (min x a)))))
	(macrolet ((iter (mid0 c0 l0 r0 &body body)
		     `(loop for (,mid0 ,c0 ,l0 ,r0) in (slot-value hist '%plot-data)
			    do (progn ,@body))))
	  (let* ((total (reduce #'+ (mapcar #'second (slot-value hist '%plot-data))
				:initial-value 0.0))
		 (total (if (zerop total) 1.0 total)))
	    (case %histogram-style
	    (:point
	     (iter mid0 c0 l0 r0
		   (let* ((mid (constrain x-min mid0 x-max))
			  (c0  (float (/ c0 total)))
			  (c   (constrain y-min c0   y-max)))
		     (draw-point hist mid c
				 :pen-width %histogram-pen-width
				 :color     %histogram-color))))
	    (:line
	     (iter mid0 c0 l0 r0
		   (let* ((mid (constrain x-min mid0 x-max))
			  (c0  (float (/ c0 total)))
			  (c   (constrain y-min c0   y-max)))
		     (draw-line  hist mid 0 mid c
				 :pen-width %histogram-line-width
				 :color     %histogram-color)
		     (draw-point hist mid c
				 :pen-width   %histogram-pen-width
				 :point-style %histogram-point-style
				 :color       %histogram-color))))
	    (otherwise
	     (iter mid0 c0 l0 r0
		   (let* ((l  (constrain x-min l0 x-max))
			  (r  (constrain x-min r0 x-max))
			  (c0 (float (/ c0 total)))
			  (c  (constrain y-min c0 y-max)))
		       (draw-rect hist l c r 0
				  :fill-color %histogram-color
				  :pen-width  %histogram-line-width
				  :fill?      t)))))))))))
