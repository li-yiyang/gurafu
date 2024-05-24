(in-package :gurafu/plot)

;; ========== legend-bar ==========

(defclass framed-margin-mixin (margined-mixin)
  ((%frame-width :initform 1
                 :initarg :frame-width)))

(defmethod present :before ((box framed-margin-mixin))
  (draw-rect box 0.0 0.0 1.0 1.0
             :fill? t
             :pen-width 0
             :color *background-color*))

(defmethod present :after ((box framed-margin-mixin))
  (draw-rect box 0.0 0.0 1.0 1.0
             :fill? nil
             :pen-width (slot-value box '%frame-width)))

;; ========== legend-bar ==========

(defclass legend-bar (base-presentation
                      margined-mixin)
  ((%color :initform *foreground-color*
           :initarg :color)
   (%style :initform *line-style*
           :initarg :style)
   (%size  :initform 2
           :initarg :size)
   (%width :initform 10
           :initarg :width)
   (%label :initform ""
           :initarg :label)
   (%space :initform 10
           :initarg :space))
  (:documentation
   "Present a `legend-bar'.

This would draw a line first, postfixed with its `%label'.
The line should be in `%color', `%style' at `%width' length,
the `%size' could be used to mimic different plot type.

For example:

    | width | space | label
    ---------        %label

The `legend-bar' size should be calculated by:

  width = %width + %space + %label's width
  height = min{ %size, %label's height }
"))

;; ========== initialize-instance ==========

(defmethod initialize-instance :after
    ((legend-bar legend-bar) &key (margin 0 margin-set?))
  (declare (ignore margin))
  (unless margin-set? (set-stream-margins legend-bar 15 10 5 5))
  (with-slots (%color %style %size %width %label %space)
      legend-bar
    (unless (stringp %label)
      (setf %label (format nil "~a" %label)))
    ;; set `legend-bar' size
    ;; width = %width + %space + %label's width
    ;; height = min{ %size, %label's height }
    (multiple-value-bind (width height)
        (draw-text-size legend-bar %label)
      (multiple-value-bind (left right bottom top)
          (stream-box legend-bar)
        (declare (ignore right bottom))
        (set-stream-box legend-bar
          left (+ left %width %space width)
          (+ top (max %size height)) top)))))

;; ========== present ==========

(defmethod present ((legend-bar legend-bar))
  (with-slots (%color %style %size %width %label %space)
      legend-bar
    (let ((half-h (truncate (stream-box-height legend-bar) 2)))
      (draw-line legend-bar 0 half-h %width half-h
                 :line-style %style
                 :pen-width  %size
                 :color      %color)
      (draw-text legend-bar (+ %width %space) half-h %label
                 :text-align :left-vertical-center))))

;; ========== legend ==========

(defclass legend (vertical-flow-layout-presentation
                  framed-margin-mixin)
  ((%legend-bars :initarg :legend-bars
                 :initform ()))
  (:documentation
   "This present a legend on plot. "))

(defmethod initialize-instance :after
    ((legend legend) &key)
  (set-stream-box legend 0 0 0 0) ;; initialize with empty box
  (with-slots (%legend-bars) legend
    (loop for (label . description) in %legend-bars
          do (add-component legend label
                            (apply #'make-instance 'legend-bar
                                   `(:label ,label ,@description))))))

;; ========== add-plot-legend ==========

(defmacro add-plot-legend ((plot &key (position :top-left)
                                   (name 'legend) (frame-width 1)
                                   (padding 0.05))
                           &body legend-bars)
  "Add legend to `plot'. "
  (with-gensyms (legend)
    (let ((legend-bars `(list ,@(loop for define in legend-bars
                                      collect (cons 'list define))))
          (u-w (ecase position
                 ((:bottom :top :bottom-left :top-left)
                  padding)
                 ((:bottom-right :top-right)
                  `(let ((inner-width (stream-box-width ,plot)))
                     (float (- (/ (- inner-width (stream-box-width ,legend))
                                  inner-width)
                               ,padding))))))
          (v-w (ecase position
                 ((:top :top-left :top-right)
                  padding)
                 ((:bottom :bottom-left :bottom-right)
                  `(let ((inner-height (stream-box-height ,plot)))
                     (float (- (/ (- inner-height (stream-box-height ,legend))
                                  inner-height)
                               ,padding)))))))
      `(let ((,legend (make-instance
                       'legend
                       :frame-width ,frame-width
                       :legend-bars ,legend-bars)))
         (add-to-plot-decorator
          ,plot ',name ,u-w ,v-w ,legend nil)))))
