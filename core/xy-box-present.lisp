(in-package :gurafu/core)

;; ========== xy-box-mixin ==========

(defclass xy-box-mixin ()
  ((%x-min :initform 0.0 :initarg :x-min)
   (%x-max :initform 1.0 :initarg :x-max)
   (%y-min :initform 0.0 :initarg :y-min)
   (%y-max :initform 1.0 :initarg :y-max)
   
   (%xy-to-uv-trans :reader %xy-to-uv-trans)
   (%uv-to-xy-trans :reader %uv-to-xy-trans))
  (:documentation
   "This class defines how th coordinate is transformed,
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

(flet ((update-xy-to-uv (stream)
         (with-slots (%x-min %x-max %y-min %y-max) stream
           (multiple-value-bind (left right bottom top)
               (stream-box stream)
             (let* ((x-scale (float (/ (- right left) (- %x-max %x-min))))
                    (y-scale (float (/ (- bottom top) (- %y-max %y-min))))
                    (u0 (- left   (truncate (* x-scale %x-min))))
                    (v0 (+ bottom (truncate (* y-scale %y-min)))))
               (declare (integer u0 v0))
               (setf (slot-value stream '%xy-to-uv-trans)
                     (lambda (x y)
                       (values (+ u0 (truncate (* x-scale x)))
                               (- v0 (truncate (* y-scale y))))))))))
       
       (update-uv-to-xy (stream)
         (with-slots (%x-min %x-max %y-min %y-max) stream
           (multiple-value-bind (left right bottom top)
               (stream-box stream)
             (let* ((u-scale (float (/ (- %x-max %x-min) (- right left))))
                    (v-scale (float (/ (- %y-max %y-min) (- bottom top))))
                    (x0 (- %x-min (* u-scale left)))
                    (y0 (+ %y-max (* v-scale top))))
               (declare (float x0 y0))
               (setf (slot-value stream '%uv-to-xy-trans)
                     (lambda (u v)
                       (values (+ x0 (* u-scale u))
                               (- y0 (* v-scale v))))))))))
  
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

  ;; ========== initialize-instance ==========
  (defmethod initialize-instance :after
      ((obj xy-box-mixin) &key)
    (update-xy-to-uv obj)
    (update-uv-to-xy obj))

  ;; ========== set-stream-box ==========

  (defmethod set-stream-box :after
      ((obj xy-box-mixin) left right bottom top)
    (declare (ignore left right bottom top))
    (update-xy-to-uv obj)
    (update-uv-to-xy obj)))

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

(defclass xy-box-present (base-presentation
                          xy-box-mixin)
  ()
  (:documentation
   ""))

(defmethod draw-point ((obj xy-box-present) x y
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

(defmethod draw-text ((obj xy-box-present) x y text
                      &key (color *foreground-color*)
                        (text-path '(1.0 1.0))
                        (text-align :normal)
                        (font-size 16)
                        (font-name "UNIFONT")
                        (char-spacing 1.0)
                        (line-width 0 line-width-set?)
                        (line-spacing 1.5)
                      &allow-other-keys)
  (with-xy-to-uv obj
      ((u v) (x y)
       (path-u path-v) ((first text-path) (second text-path)))
    (if line-width-set?
        (draw-text! (slot-value obj '%backend) u v text
                    :color        color
                    :text-path    (list path-u path-v)
                    :text-align   text-align
                    :font-size    font-size
                    :font-name    font-name
                    :char-spacing char-spacing
                    :line-width   line-width
                    :line-spacing line-spacing)
        (draw-text! (slot-value obj '%backend) u v text
                    :color        color
                    :text-path    (list path-u path-v)
                    :text-align   text-align
                    :font-size    font-size
                    :font-name    font-name
                    :char-spacing char-spacing))))

(defmethod draw-rect ((obj xy-box-present) x1 y1 x2 y2
                      &key (color *foreground-color*)
                        (pen-width 1)
                        (line-style :solid)
                        (fill? t)
                        (fill-color color)
                      &allow-other-keys)
  (with-xy-to-uv obj
      ((u1 v1) (x1 y1)
       (u2 v2) (x2 y2))
    (draw-rect! (slot-value obj '%backend)
                (min u1 u2) (min v1 v2)
                (max u1 u2) (max v1 v2)
                :fill-color fill-color
                :fill?      fill?
                :line-style line-style
                :pen-width  pen-width
                :color      color)))

(defmethod draw-line ((obj xy-box-present) x1 y1 x2 y2
                      &key (color *foreground-color*)
                        (pen-width 1)
                        (line-style :solid)
                      &allow-other-keys)
  (with-xy-to-uv obj
      ((u1 v1) (x1 y1)
       (u2 v2) (x2 y2))
    (draw-line! (slot-value obj '%backend) u1 v1 u2 v2
                :line-style line-style
                :pen-width  pen-width
                :color      color)))
