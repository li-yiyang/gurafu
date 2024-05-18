(in-package :gurafu/plot)

;; ========== label ==========

(defclass label (base-presentation)
  ((%label        :initform ""
                  :initarg :label)
   (%color        :initform *foreground-color*
                  :initarg :color)
   (%font-size    :initform *font-size*
                  :initarg :font-size)   
   (%line-forward :initform *line-forward*)
   (%char-forward :initform *char-forward*)
   (%line-spacing :initform *line-spacing*)
   (%char-spacing :initform *char-spacing*))
  (:documentation
   "To present a text label (`:normal' text-align). "))

;; ========== initialize-instance ==========
;; reset label height to text height after initialize

(defmethod initialize-instance :after
    ((label label) &key (text-path '(1.0 0.0) text-path-set?))
  (with-slots (%label %color %font-size
               %line-forward %char-forward
               %line-spacing %char-spacing
               %text-align)
      label
    (when text-path-set?
      (setf %char-forward text-path
            %line-forward (reverse text-path)))
    (multiple-value-bind (width height)
        (let ((*char-forward* %char-forward)
              (*line-forward* %line-forward)
              (*line-spacing* %line-spacing)
              (*char-spacing* %char-spacing)
              (*font-size*    %font-size))
          (draw-text-size label %label                          
                          :line-width (stream-box-width label)))      
      (declare (ignore width))
      (multiple-value-bind (left right bottom top)
          (stream-box label)
        (set-stream-box label left right (+ height bottom) top)))))

;; ========== present ==========

(defmethod present ((label label))
  (with-slots (%label %color %font-size) label
    (draw-text label 0 0 %label
               :color %color
               :font-size %font-size
               :text-align :normal
               :line-width (stream-box-width label))))
