(in-package :gurafu/core)

(defparameter *x-min* 0.0
  "Default x-min value. ")

(defparameter *x-max* 1.0
  "Default x-min value. ")

(defparameter *y-min* 0.0
  "Default x-min value. ")

(defparameter *y-max* 1.0
  "Default x-min value. ")

;; ========== xy-box-mixin ==========

(defclass xy-box-mixin ()
  ((%x-min :initform *x-min* :initarg :x-min)
   (%x-max :initform *x-max* :initarg :x-max)
   (%y-min :initform *y-min* :initarg :y-min)
   (%y-max :initform *y-max* :initarg :y-max)
   
   (%xy-to-uv-trans :reader %xy-to-uv-trans)
   (%uv-to-xy-trans :reader %uv-to-xy-trans))
  (:documentation
   "This class defines how the coordinate is transformed,
which handles how the user set position `x', `y' is mapped
to digital plotting `u', `v'.

When using `xy-box-mixin', the `draw-*' method will use the
`x', `y' coordinates rather than `u' and `v'.

The `xy-box-mixin' provides quick access macros for coordinates
transformation `with-xy-to-uv' and `with-uv-to-xy'. 

By normal, the coordinate is like below:

  *-----------> u                   ^ y
  |        stream-pos!              |                 max
  | left  /                         |      +--------+ 
  | top  +--------+                 |      | stream |
  |      | stream | stream-height!  | min  +--------+
  | v    +--------+    right        |       \ stream-xy-pos
  V      stream-width! bottom       *-------------------> x

The slot value `%x-min', `%x-max', `%y-min', `%y-max' marks
the bounding-box in xy space; `%xy-to-uv-trans',
`%uv-to-xy-trans' is a function that trans points.

The `%xy-to-uv-trans':

  x-scale = (uv-right - uv-left) / (xy-right - xy-left)
  y-scale = (uv-bottom - uv-top) / (xy-top - xy-bottom)

  u = uv-left + x-scale * (x - xy-left)
    => (uv-left - x-scale * xy-left) + x-scale * x
  v = uv-bottom - y-scale * (y - xy-bottom)
    => (uv-bottom + y-scale * xy-bottom) - y-scale * y

The `%uv-to-xy-trans':

  u-scale = (xy-right - xy-left) / (uv-right - uv-left)
  v-scale = (xy-top - xy-bottom) / (uv-bottom - uv-top)

  x = xy-left + u-scale * (u - uv-left)
    => (xy-left - u-scale * uv-left) + u-scale * u
  y = xy-top - v-scale * (v - uv-top)
    => (xy-top + v-scale * uv-top) - v-scale * v

"))

;; ========== xy-bounding-box ==========

(defgeneric xy-bounding-box (stream)
  (:documentation
   "Get the xy bounding box for `stream'.
Return values are `x-min', `x-max', `y-min', `y-max'. "))

(defmethod xy-bounding-box ((stream xy-box-mixin))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (values %x-min %x-max %y-min %y-max)))

;; ========== set-xy-bounding-box ==========

(defgeneric set-xy-bounding-box
    (stream x-min x-max y-min y-max)
  (:documentation
   "Set the xy bounding box for `stream'. "))

(defgeneric update-uv-to-xy (stream)
  (:documentation "Update the uv to xy transformations. "))

(defgeneric update-xy-to-uv (stream)
  (:documentation "Update the xy to uv transformations. "))

(defun make-xy-to-uv-trans (%x-min %x-max %y-min %y-max
                            left right bottom top)
  "Make a xy to uv transformer function. "
  )

(defun make-uv-to-xy-trans (%x-min %x-max %y-min %y-max
                            left right bottom top)
  "Make a uv to xy transformer function. "
  )

(defmethod update-xy-to-uv ((stream xy-box-mixin))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (setf (slot-value stream '%xy-to-uv-trans)
            (let* ((x-scale (float (/ (- right left) (- %x-max %x-min))))
                   (y-scale (float (/ (- bottom top) (- %y-max %y-min))))
                   (u0 (- left   (truncate (* x-scale %x-min))))
                   (v0 (+ bottom (truncate (* y-scale %y-min)))))
              (declare (integer u0 v0))
              (lambda (x y)
                (values (+ u0 (truncate (* x-scale x)))
                        (- v0 (truncate (* y-scale y))))))))))

(defmethod update-uv-to-xy ((stream xy-box-mixin))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (setf (slot-value stream '%uv-to-xy-trans)
            (let* ((u-scale (float (/ (- %x-max %x-min) (- right left))))
                   (v-scale (float (/ (- %y-max %y-min) (- bottom top))))
                   (x0 (- %x-min (* u-scale left)))
                   (y0 (+ %y-max (* v-scale top))))
              (declare (float x0 y0))
              (lambda (u v)
                (values (+ x0 (* u-scale u))
                        (- y0 (* v-scale v)))))))))

;; ========== initialize-instance ==========
(defmethod initialize-instance :after
    ((obj xy-box-mixin) &key)
  (update-xy-to-uv obj)
  (update-uv-to-xy obj))

;; ========== set-xy-bounding-box ==========
(defmethod set-xy-bounding-box
    ((stream xy-box-mixin) x-min x-max y-min y-max)
  (with-slots (%x-min %x-max %y-min %y-max) stream
    ;; update xy bounding box
    (setf %x-min x-min
          %x-max x-max
          %y-min y-min
          %y-max y-max)
    
    ;; update `%xy-to-uv-trans'
    (update-xy-to-uv stream)

    ;; update `%uv-to-xy-trans'
    (update-uv-to-xy stream)))

;; ========== set-stream-box ==========
(defmethod set-stream-box :after
    ((obj xy-box-mixin) left right bottom top)
  (declare (ignore left right bottom top))
  (update-xy-to-uv obj)
  (update-uv-to-xy obj))

;; ========== x-log-xy-box-mixin ==========

(defclass x-log-xy-box-mixin (xy-box-mixin) ()
  (:documentation "The x coordinate is transformed in log scale. "))

(defmethod update-uv-to-xy ((stream x-log-xy-box-mixin))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (let ((%x-min (log %x-min))
            (%x-max (log %x-max)))
        (setf (slot-value stream '%uv-to-xy-trans)
              (let* ((u-scale (float (/ (- %x-max %x-min) (- right left))))
                     (v-scale (float (/ (- %y-max %y-min) (- bottom top))))
                     (x0 (- %x-min (* u-scale left)))
                     (y0 (+ %y-max (* v-scale top))))
                (declare (float x0 y0))
                (lambda (u v)
                  (values (exp (+ x0 (* u-scale u)))
                          (- y0 (* v-scale v))))))))))

(defmethod update-xy-to-uv ((stream x-log-xy-box-mixin))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (let ((%x-min (log %x-min))
            (%x-max (log %x-max)))
        (setf (slot-value stream '%xy-to-uv-trans)
              (let* ((x-scale (float (/ (- right left) (- %x-max %x-min))))
                     (y-scale (float (/ (- bottom top) (- %y-max %y-min))))
                     (u0 (- left   (truncate (* x-scale %x-min))))
                     (v0 (+ bottom (truncate (* y-scale %y-min)))))
                (declare (integer u0 v0))
                (lambda (x y)
                  (values (+ u0 (truncate (* x-scale (log x))))
                          (- v0 (truncate (* y-scale y)))))))))))

;; ========== y-log-xy-box-mixin ==========

(defclass y-log-xy-box-mixin (xy-box-mixin) ()
  (:documentation "The y coordinate is transformed in log scale. "))

(defmethod update-uv-to-xy ((stream y-log-xy-box-mixin))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (let ((%y-min (log %y-min))
            (%y-max (log %y-max)))
        (setf (slot-value stream '%uv-to-xy-trans)
              (let* ((u-scale (float (/ (- %x-max %x-min) (- right left))))
                     (v-scale (float (/ (- %y-max %y-min) (- bottom top))))
                     (x0 (- %x-min (* u-scale left)))
                     (y0 (+ %y-max (* v-scale top))))
                (declare (float x0 y0))
                (lambda (u v)
                  (values (+ x0 (* u-scale u))
                          (exp (- y0 (* v-scale v)))))))))))

(defmethod update-xy-to-uv ((stream y-log-xy-box-mixin))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (let ((%y-min (log %y-min))
            (%y-max (log %y-max)))
        (setf (slot-value stream '%xy-to-uv-trans)
              (let* ((x-scale (float (/ (- right left) (- %x-max %x-min))))
                     (y-scale (float (/ (- bottom top) (- %y-max %y-min))))
                     (u0 (- left   (truncate (* x-scale %x-min))))
                     (v0 (+ bottom (truncate (* y-scale %y-min)))))
                (declare (integer u0 v0))
                (lambda (x y)
                  (values (+ u0 (truncate (* x-scale x)))
                          (- v0 (truncate (* y-scale (log y))))))))))))

;; ========== log-log-xy-box-mixin ==========

(defclass log-log-xy-box-mixin (xy-box-mixin) ()
  (:documentation "The x, y coordinates are transformed in log scale. "))

(defmethod update-uv-to-xy ((stream log-log-xy-box-mixin))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (let ((%x-min (log %x-min))
            (%x-max (log %x-max))
            (%y-min (log %y-min))
            (%y-max (log %y-max)))
        (setf (slot-value stream '%uv-to-xy-trans)
              (let* ((u-scale (float (/ (- %x-max %x-min) (- right left))))
                     (v-scale (float (/ (- %y-max %y-min) (- bottom top))))
                     (x0 (- %x-min (* u-scale left)))
                     (y0 (+ %y-max (* v-scale top))))
                (declare (float x0 y0))
                (lambda (u v)
                  (values (exp (+ x0 (* u-scale u)))
                          (exp (- y0 (* v-scale v)))))))))))

(defmethod update-xy-to-uv ((stream log-log-xy-box-mixin))
  (with-slots (%x-min %x-max %y-min %y-max) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (let ((%x-min (log %x-min))
            (%x-max (log %x-max))
            (%y-min (log %y-min))
            (%y-max (log %y-max)))
        (setf (slot-value stream '%xy-to-uv-trans)
              (let* ((x-scale (float (/ (- right left) (- %x-max %x-min))))
                     (y-scale (float (/ (- bottom top) (- %y-max %y-min))))
                     (u0 (- left   (truncate (* x-scale %x-min))))
                     (v0 (+ bottom (truncate (* y-scale %y-min)))))
                (declare (integer u0 v0))
                (lambda (x y)
                  (values (+ u0 (truncate (* x-scale (log x))))
                          (- v0 (truncate (* y-scale (log y))))))))))))

;; ========== with-xy-to-uv ==========

(defmacro with-xy-to-uv (stream bindings &body body)
  "Bind the transformed variables from xy to uv. 

Example:

  (with-xy-to-uv stream
      ((u1 v1) (x1 y1)
       (u2 v2) (x2 y2))
    (do-some-thing))
"
  (if (endp bindings)
      `(progn ,@body)
      `(multiple-value-bind ,(first bindings)
           (funcall (%xy-to-uv-trans ,stream) ,@(second bindings))
         (with-xy-to-uv ,stream ,(cddr bindings) ,@body))))

(defmacro with-uv-to-xy (stream bindings &body body)
  "Bind the transformed variables from uv to xy. 

Example:

  (with-uv-to-xy stream
      ((x1 y1) (u1 v1) 
       (x2 y2) (u2 v2))
    (do-some-thing))
"
  (if (endp bindings)
      `(progn ,@body)
      `(multiple-value-bind ,(first bindings)
           (funcall (%uv-to-xy-trans ,stream) ,@(second bindings))
         (with-xy-to-uv ,stream ,(cddr bindings) ,@body))))

;; ========== draw-* functions ==========

(defclass box-present            (base-presentation) ())

(defclass xy-box-present         (box-present xy-box-mixin)         ())
(defclass log-log-xy-box-present (box-present log-log-xy-box-mixin) ())
(defclass x-log-xy-box-present   (box-present x-log-xy-box-mixin)   ())
(defclass y-log-xy-box-present   (box-present y-log-xy-box-mixin)   ())

(defmethod draw-point ((obj box-present) x y
                       &key (color *foreground-color*)
                         (point-style :dot)
                         (pen-width 1)
                       &allow-other-keys)
  (with-xy-to-uv obj
      ((u v) (x y))
    (draw-point! (slot-value obj '%backend) u v
                 :point-style point-style
                 :pen-width   pen-width
                 :color       color)))

(defmethod draw-text ((obj box-present) x y text
                      &key (color *foreground-color*)
                        (text-path '(1.0 1.0) text-path-set?)
                        (line-forward *line-forward*)
                        (char-forward *char-forward*)
                        (text-align *text-align*)
                        (font-size *font-size*)
                        (font-name "UNIFONT")
                        (char-spacing *char-spacing*)
                        (line-spacing *line-spacing*)
                        (line-width 0)
                      &allow-other-keys)
  (with-xy-to-uv obj
      ((u v) (x y)
       (path-u path-v) ((first text-path) (second text-path)))
    (let ((*line-spacing* line-spacing)
          (*char-spacing* char-spacing)
          (*foreground-color* color)
          (*text-align* text-align)
          (*char-forward* (if text-path-set? (list path-u path-v) char-forward))
          (*line-forward* (if text-path-set? (list path-v path-u) line-forward))
          (*font-size* font-size))
      (draw-text! (slot-value obj '%backend) u v text
                  :font-name    font-name
                  :line-width   line-width))))

(defmethod draw-rect ((obj box-present) x1 y1 x2 y2
                      &key (color *foreground-color*)
                        (pen-width *pen-width*)
                        (line-style *line-style*)
                        (fill? *fill?*)
                        (fill-color color)
                      &allow-other-keys)
  (with-xy-to-uv obj
      ((u1 v1) (x1 y1)
       (u2 v2) (x2 y2))
    (let ((*line-style* line-style)
          (*fill?* fill?)
          (*pen-width*  pen-width)
          (*foreground-color* color))
      (draw-rect! (slot-value obj '%backend)
                (min u1 u2) (min v1 v2)
                (max u1 u2) (max v1 v2)
                :fill-color fill-color))))

(defmethod draw-line ((obj box-present) x1 y1 x2 y2
                      &key (color *foreground-color*)
                        (pen-width *pen-width*)
                        (line-style *line-style*)
                      &allow-other-keys)
  (with-xy-to-uv obj
      ((u1 v1) (x1 y1)
       (u2 v2) (x2 y2))
    (let ((*line-style* line-style)
          (*pen-width*  pen-width)
          (*foreground-color* color))
      (draw-line! (slot-value obj '%backend) u1 v1 u2 v2))))
