(in-package :gurafu/plot)

(defclass framed-axes-mixin (margined-mixin)
  ((%x-ticks          :initform 3
                      :initarg :x-ticks)
   (%y-ticks          :initform 3
                      :initarg :y-ticks)
   (%tick-precisition :initform 2
                      :initarg :tick-precise)
   (%tick-font-size   :initform 12)
   (%tick-font-color  :initform +gray+)
   (%tick-height      :initform 4)
   (%tick-padding     :initform 8)
   (%tick-color       :initform +black+)
   (%frame-width      :initform 2
                      :initarg :frame-width)
   (%x-label          :initform "x"
                      :initarg :x-label)
   (%y-label          :initform "y"
                      :initarg :y-label)))

(defmethod set-stream-margins :around
    ((plot framed-axes-mixin) left right bottom top)
  (with-slots (%tick-padding %tick-font-size
               %y-min %y-max %tick-precisition)
      plot
    (flet ((precise (x)
             (format nil (format nil "~~,~df" %tick-precisition) x)))
      (let* ((offset (+ %tick-padding %tick-font-size))
             (left-offset (max offset
                               (draw-text-size plot (precise %y-max))
                               (draw-text-size plot (precise %y-min)))))
        (call-next-method plot
                          (+ left left-offset) (+ right offset)
                          (+ bottom offset) (+ top offset))))))

(defmethod present :after ((plot framed-axes-mixin))
  (with-slots (%frame-width %x-label %y-label
               %tick-font-size %tick-padding
               %tick-precisition %tick-height
               %tick-color %tick-font-color
               %x-ticks %y-ticks)
      plot
    ;; draw y-label
    (draw-text plot 0.0 0.0 %y-label
               :font-size %tick-font-size
               :text-align :right-bottom
               :offset (list (- %tick-padding) (- %tick-padding)))
    
    ;; draw x-label
    (draw-text plot 1.0 1.0 %x-label
               :font-size %tick-font-size
               :text-align :left-bottom
               :offset (list %tick-padding 0))

    ;; draw-ticks
    (multiple-value-bind (left right bottom top)
        (stream-box plot)
      (multiple-value-bind (xmin xmax ymin ymax)
          (xy-bounding-box plot)
        (flet ((precise (x)
                 (format nil (format nil "~~,~df" %tick-precisition) x)))
          ;; draw x ticks with coordinates
          (loop with width = (float (/ (- right left) (1+ %x-ticks)))
                with height = (- bottom top)
                with xwidth = (float (/ (- xmax xmin) (1+ %x-ticks)))
                
                for i upto (1+ %x-ticks)
                for u = (truncate (* width i))
                for x = (+ xmin (* xwidth i))

                do (draw-line plot u (- height %tick-height) u height
                              :color %tick-color)
                do (draw-text plot u (+ height %tick-padding) (precise x)
                              :text-align :top-center
                              :font-size  %tick-font-size
                              :color      %tick-font-color))
          ;; draw y ticks with coordinates
          (loop with height = (float (/ (- bottom top) (1+ %y-ticks)))
                with yheight = (float (/ (- ymax ymin) (1+ %y-ticks)))

                for i upto (1+ %y-ticks)
                for v = (- (- bottom top) (truncate (* height i)))
                for y = (+ ymin (* yheight i))

                do (draw-line plot 0 v %tick-height v
                              :color %tick-color)
                do (draw-text plot (- %tick-padding) v (precise y)
                              :text-align :right-vertical-center
                              :font-size  %tick-font-size
                              :color      %tick-font-color)))))

    (when (> %frame-width 0)      
      ;; draw boundary frame
      (draw-rect plot 0.0 0.0 1.0 1.0
                 :color +black+
                 :pen-width %frame-width
                 :fill? nil))))

;; ========== plot ==========

(defclass plot (stack-layout-presentation
                framed-axes-mixin)
  ((%x-min :initform -1 :initarg :x-min)
   (%x-max :initform 1  :initarg :x-max)
   (%y-min :initform 1  :initarg :y-min)
   (%y-max :initform 1  :initarg :y-max))
  (:documentation
   "This is the basic plot holder to show a plot.

The `plot' is a collection of `plot-pane' which draws the "))

;; ========== xy-bounding-box ==========

(defmethod xy-bounding-box ((stream plot))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (values %x-min %x-max %y-min %y-max)))

;; ========== set-xy-bounding-box ==========

(defmethod set-xy-bounding-box ((stream plot) x-min x-max y-min y-max)
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (setf %x-min x-min
          %x-max x-max
          %y-min y-min
          %y-max y-max)
    (loop-components (stream plot-pane)
      (set-xy-bounding-box plot-pane x-min x-max y-min y-max))))

;; ========== rescale-plot-pane ==========

(defmethod rescale-plot-pane ((plot plot))
  (with-slots (%x-min %x-max %y-min %y-max) plot
    (loop-components (plot plot-pane)
      (multiple-value-bind (x-min x-max y-min y-max)
          (xy-bounding-box plot-pane)
        (setf %x-min (min %x-min x-min)
              %x-max (max %x-max x-max)
              %y-min (min %y-min y-min)
              %y-max (max %y-max y-max))))
    (set-xy-bounding-box plot %x-min %x-max %y-min %y-max)))

;; ========== add-plot-pane ==========

(defgeneric add-plot-pane (plot name plot-pane)
  (:documentation
   "Add a `plot-pane' with `name' to `plot'. "))

(defmethod add-plot-pane ((plot plot) name plot-pane)
  (add-component plot name plot-pane 0.0 0.0 1.0 1.0)
  (rescale-plot-pane plot))

;; ========== add-plot-data ==========

(defgeneric add-plot-data (plot name data &key &allow-other-keys)
  (:documentation
   "Add a `data' to `plot' as a plot-pane named `name'. "))

(defmethod add-plot-data ((plot plot) name data
                          &key (pane-type 'line-plot-pane)
                            (color *foreground-color*)
                          &allow-other-keys)
  (let ((pane (make-instance pane-type
                             :color color
                             :plot-data data)))
    (add-plot-pane plot name pane)))

;; ========== get-plot-pane ==========

(defgeneric get-plot-pane (plot name)
  (:documentation
   "Get the plot-pane with `name' in `plot'. "))

(defmethod get-plot-pane ((plot plot) name)
  (gethash name (slot-value plot '%components)))
