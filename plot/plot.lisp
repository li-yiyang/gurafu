(in-package :gurafu/plot)

;; ========== framed-axes-mixin ==========

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
                      :initarg :y-label))
  (:documentation
   "The `framed-axes-mixin' is subclass of `margined-mixin'.
It add a margin-like feature to the object, providing"))

(defmethod set-stream-margins :around
    ((plot framed-axes-mixin) left right bottom top)
  (with-slots (%tick-padding %tick-font-size
               %tick-precisition
               %y-label %x-label)
      plot
    (multiple-value-bind (x-min x-max y-min y-max)
        (xy-bounding-box plot)
      (declare (ignore x-min x-max))
      (flet ((precise (x)
               (format nil (format nil "~~,~df" %tick-precisition) x)))
        (let* ((offset (+ %tick-padding %tick-font-size))
               (left-offset (+ %tick-padding
                               (max %tick-font-size
                                    (draw-text-size plot (precise y-max))
                                    (draw-text-size plot (precise y-min))
                                    (draw-text-size plot %y-label))))
               (right-offset (+ %tick-padding
                                (draw-text-size plot %x-label))))
          (call-next-method plot
                            (+ left left-offset) (+ right right-offset)
                            (+ bottom offset) (+ top offset)))))))

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

;; ========== plot-panes ==========

(defclass plot-panes (stack-layout-presentation
                      xy-box-mixin)
  ()
  (:documentation
   "The `plot-panes' is a collection of `plot-pane' objects.
The `plot-panes' is `stack-layout-presentation' subclass object,
its component of `plot-panes' should be `plot-pane'. "))

;; ========== set-xy-bounding-box ==========
;; reset inner `plot-pane' object xy-bounding-box

(defmethod set-xy-bounding-box :after
    ((plot-panes plot-panes) x-min x-max y-min y-max)
  (loop-components (plot-panes plot-pane)
    (set-xy-bounding-box plot-pane x-min x-max y-min y-max)))

;; ========== rescale-plot-pane ==========

(defmethod rescale-plot-pane ((plot-panes plot-panes))
  (multiple-value-bind (x-min x-max y-min y-max)
      (xy-bounding-box plot-panes)
    (loop-components (plot-panes plot-pane)
      (multiple-value-bind (a-min a-max b-min b-max)
          (xy-bounding-box plot-pane)
        (setf x-min (min x-min a-min)
              x-max (max x-max a-max)
              y-min (min y-min b-min)
              y-max (max y-max b-max))))
    (set-xy-bounding-box plot-panes x-min x-max y-min y-max)))

;; ========== add-plot-pane ==========

(defgeneric add-plot-pane (plot name plot-pane)
  (:documentation
   "Add a `plot-pane' with `name' to `plot'. "))

(defmethod add-plot-pane ((plot-panes plot-panes) name plot-pane)
  (unless (typep plot-pane 'basic-plot-pane)
    (warn (format nil "~a may not be subclass of `basic-plot-pane'."
                  (type-of plot-pane))))
  (add-component plot-panes name plot-pane 0.0 0.0 1.0 1.0)
  (rescale-plot-pane plot-panes))

;; ========== plot ==========

(defclass plot (base-presentation
                framed-axes-mixin)
  ((%plot-panes :initform (make-instance 'plot-panes))
   (%decorators :initform (make-instance 'stack-layout-presentation))
   (%background-color :initform *background-color*
                      :initarg :background-color))
  (:documentation
   "This is the basic plot holder to show a plot.

The `plot' is like a four layer image, from bottom to top, there are:

1. background: a rectangle filled with `%background-color';
2. plot-panes: a collection of `plot-pane' holding different plot in
   the same xy box, the last added plot-pane will be drawn on top;
3. decorator: a collection of widgets, which are drawn on the top;
4. frame ticks: from `framed-axes-mixin'.

===== PLOT-PANE ====

To add a data to `plot', use `add-plot-pane' or `add-plot-data':

+ `add-plot-pane' is like `add-component', but using `add-plot-pane'
  is recommanded.

  for example:

    (add-plot-pane plot NAME (with-present (2d-grid-pane :plot-data ...)))

  this is a little difficult to use, so anyway, I recommand to use
  `add-plot-data' macro to do this automatically;

+ `add-plot-data' is a warpper macro for `add-plot-pane'.

  for example:

    (add-plot-data plot (2d-grid-pane NAME :init-args ...)
       code-to-generate-the-plot-data)

  the code body of `add-plot-data' should return a plot data for `plot-pane'.

===== DECORATORS =====

"))

;; ========== initialize-instance ==========

(flet ((re-align (plot left right bottom top)
         (with-slots (%decorators %plot-panes) plot
           (set-stream-bounding-box %decorators left right bottom top)
           (set-stream-bounding-box %plot-panes left right bottom top))))
  (defmethod initialize-instance :after
      ((plot plot) &key (x-min -1) (x-max 1) (y-min -1) (y-max 1))
    ;; set xy-bounding-box for `%plot-panes'
    (set-xy-bounding-box (slot-value plot '%plot-panes)
                         x-min x-max y-min y-max)
    ;; re-align inner `%plot-panes' and `%decorators'
    (multiple-value-bind (left right bottom top)
        (stream-box plot)
      (re-align plot left right bottom top)
      ))
  
  (defmethod set-stream-box :after
      ((plot plot) left right bottom top)
    (re-align plot left right bottom top)))

;; ========== xy-bounding-box ==========

(defmethod xy-bounding-box ((plot plot))
  (xy-bounding-box (slot-value plot '%plot-panes)))

(defmethod set-xy-bounding-box ((plot plot) x-min x-max y-min y-max)
  (set-xy-bounding-box (slot-value plot '%plot-panes)
                       x-min x-max y-min y-max))

;; ========== rescale-plot-pane ==========

(defmethod rescale-plot-pane ((plot plot))
  (rescale-plot-pane (slot-value plot '%plot-panes)))

;; ========== add-plot-pane ==========

(defmethod add-plot-pane ((plot plot) name plot-pane)
  (add-plot-pane (slot-value plot '%plot-panes) name plot-pane))

;; ========== add-plot-data ==========

(defmacro add-plot-data (plot (type name &rest init-args) &body data)
  "Add a `data' to `plot' as a plot-pane named `name'. "
  `(add-plot-pane ,plot ',name
                  (make-instance ',type :plot-data (progn ,@data)
                                 ,@init-args)))

;; ========== add-plot-decorator ==========

(defgeneric add-to-plot-decorator (plot name u-x v-y decorator &optional xy?)
  (:documentation
   "Add a `decorator' with `name' to `plot'.
If `xy?' (default `nil'), then use x-y coordinates for add plot. "))

(defmethod add-to-plot-decorator
    ((plot plot) name u-x v-y decorator &optional (xy? nil))
  (cond (xy?
         (with-xy-to-uv (slot-value plot '%plot-panes)
             ((u v) (u-x v-y))
           (multiple-value-bind (left right bottom top)
               (stream-box plot)
             (add-component (slot-value plot '%decorators)
                            name decorator
                            (float (/ (- u left) (- right left)))
                            (float (/ (- v top) (- bottom top)))))))
        ((and (floatp u-x) (floatp v-y)
              (<= 0.0 u-x 1.0) (<= 0.0 v-y 1.0))
         (add-component (slot-value plot '%decorators)
                        name decorator u-x v-y))
        (t
         (multiple-value-bind (left right bottom top)
             (stream-box plot)
           (let ((u-w (float (/ u-x (- right left))))
                 (v-w (float (/ v-y  (- bottom top)))))
             (add-component (slot-value plot '%decorators)
                            name decorator u-w v-w)
             (when (or (> u-w 1) (< u-w 0)
                       (> v-w 1) (< v-w 0))
               (warn (format
                      nil
                      "Added decorator (~,3f, ~,3f) may outside of the plot. "
                      u-w v-w))))))))

;; ========== add-plot-decorator ==========

(defmacro add-plot-decorator ((plot name type &rest coordinate) &rest init-args)
  "Add plot decorator to `plot' with `name' at `coordinate'.
The `coordinate' could be like:
+ `x', `y' (default use x-y coodrinates)
+ `:xy', `x', `y'
+ `:uv', `u', `v'
  if `u' and `v' are float between 0 and 1, will be used as a scale

The `init-args' should be used for init the decorator."
  (let ((a (first  coordinate))
        (b (second coordinate))
        (c (third  coordinate)))
    (if c
        `(add-to-plot-decorator ,plot ',name ,b ,c
                                (make-instance ',type ,@init-args)
                                ,(ecase a (:xy t) (:uv nil)))
        `(add-to-plot-decorator ,plot ',name ,a ,b
                                (make-instance ',type ,@init-args)
                                t))))

;; ========== get-plot-pane ==========

(defgeneric get-plot-pane (plot name)
  (:documentation
   "Get the plot-pane with `name' in `plot'. "))

(defmethod get-plot-pane ((plot plot) name)  
  (get-component (slot-value plot '%plot-panes) name))

;; ========== get-plot-decorator ==========

(defgeneric get-plot-decorator (plot name)
  (:documentation
   "Get the plot dcorator with `name' in `plot'. "))

(defmethod get-plot-decorator ((plot plot) name)
  (get-component (slot-value plot '%decorators) name))

;; ========== present ==========

(defmethod present ((plot plot))
  (with-slots (%background-color %plot-panes %decorators) plot
    (draw-rect plot 0.0 0.0 1.0 1.0 :color %background-color)
    (present %plot-panes)
    (present %decorators)))
