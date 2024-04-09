(in-package :gurafu/core)

;; ========== basic-plot-pane ==========

(defclass basic-plot-pane (xy-box-present)
  ((%plot-data :initform nil :initarg :plot-data))
  (:documentation
   "This is the base class for showing a plot.

The `plot-pane' is a place where `%plot-data' would be shown.
The `%plot-data' should be abitrary data sets which should be
used to draw by `present'.

All plot-pane should be class inherited from `basic-plot-pane'.
The plot-pane should hook it's own `present' methods. "))

;; ========== rescale-plot-pane ==========

(defgeneric rescale-plot-pane (plot-pane)
  (:documentation
   "Rescale the plot-pane automatically.
Return values are rescaled plot-pane xy-bounding-box. "))

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
   "Draw a line of "))

(defmethod rescale-plot-pane ((plot line-plot-pane))
  (with-slots (%plot-data) plot
    (macrolet ((got (type pos)
                 `(apply #',type (mapcar #',pos %plot-data))))
      (set-xy-bounding-box
       plot (got min first) (got max first)
       (got min second) (got max second)))))

(defmethod initialize-instance :after ((plot line-plot-pane) &key)
  (rescale-plot-pane plot))

(defmethod present ((plot line-plot-pane))
  (multiple-value-bind (x-min x-max y-min y-max)
      (xy-bounding-box plot)
    (with-slots (%line-style %line-color %line-width
                 %point-style %point-size %point-color
                 %plot-data)
        plot
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
                             :color       %point-color)))))

;; ========== ticked-mixin ==========

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

;; ========== present plot ==========

