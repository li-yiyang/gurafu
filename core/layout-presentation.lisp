(in-package :gurafu/core)

;; ========== README : about layout-presentation ==========
;; The layout presentation class:
;; 
;; +--- layout-presentation
;;   |  => stack-layout-presentation
;;   |
;;   +--- content-relative-layout-presentation
;;        => vertical-layout-presentation
;;        => horizontal-layout-presentation
;;        => vertical-flow-layout-presentation
;;        => horizontal-flow-layout-presentation
;; 
;; P.S. I should done this more neatly, though, current layout
;; system is quite dummy and slow. Need rewrite later.

;; ========== layout-presentation ==========

(defclass layout-presentation (base-presentation)
  ((%components         :initform (make-hash-table :test 'equal))
   (%components-weights :initform ()))
  (:documentation
   "A simple layout mixin.

A `layout-presentation' is a combination of other components.
All the other components are stored in `%components' as hashtable.
Every component in `layout-presentation' should have a unique name;

A `layout-presentation' will automatically slove screen coordination
relationships. All the coordinate relation is stored in
`%components-weights' as a assoc-list;

To get a component of a layout by name, use `get-component';
To add a component to a layout with name, use `add-component';
To rescale a component by name and weights, use `rescale-component';
To iter through all the components, use `loop-components' macro;
or use `loop-components-name' if only deal with the name;

Those directly inherit `layout-presentation' should hook AFTER:
+ `rescale-component' to update the `%components' coordinates;

For `present' method, every time presenting the component of a
`layout-presentation', it would iter through the `%components',
and apply `present' on each component. "))

;; ========== (present layout-presentation) ==========
;; The `present' method for `layout-presentation'
;; just present the `%components' respectively.
;; 
;; The component of `layout-presentation' should be
;; reshaped with `rescale-component' when:
;; + added to layout: `add-component'
;; + reshape the layout: `set-stream-bounding-box'

(defmethod present ((layout layout-presentation))
  (maphash-values #'present (slot-value layout '%components)))

;; ========== get-component ==========

(defgeneric get-component (layout name)
  (:documentation
   "Get the component of `stream' with `name'. "))

(defmethod get-component ((layout layout-presentation) name)
  (gethash name (slot-value layout '%components)))

;; ========== check-weights ==========
;; Different `layout-presentation' type should have different
;; weights for how the component is layouted.
;; 
;; Use `declaim-weights' to set the `layout-presentation'.

(defgeneric check-weights (layout weights)
  (:documentation
   "Check if the `weights' is valid for `layout'. "))

(defmacro declaim-weights (layout-type &rest sizes)
  "This should define the `weights' size of a specific `layout-type'. "
  `(defmethod check-weights ((layout ,layout-type) weights)
     (let ((len (length weights)))
       (unless (or ,@(mapcar (lambda (size) `(= len ,size)) sizes))
         (error ,(format nil
                         "~a need ~{~a~#[~; or ~:;, ~]~} args for `weights'"
                         layout-type sizes))))))

;; ========== add-component ==========

(defgeneric add-component (stream name component &rest weights)
  (:documentation
   "Add `component' to `stream' as `name' with `weights'.

The component name and weights will be append to the end of `%components-weights',
and the component itself will be stored into `%components' hash-table, with
its name as indexing key.

After the `component' is added to `stream', it will be resized via
`rescale-component' method. Each `layout-presentation' will know how to
rescale the compnent, making it align right. "))

(defmethod add-component
    ((stream layout-presentation) name component &rest weights)
  (check-weights stream weights)
  (with-slots (%components %components-weights) stream
      ;; register components to the end of `%components-weights'
      (setf %components-weights
            (nconc %components-weights (list (cons name weights))))

      ;; add components to the `%components'
    (setf (gethash name %components) component))
  (apply #'rescale-component stream name weights))

;; ========== rescale-component ==========
;; Update the registed weights info for component with name,
;; note that this method could be exposed to normal user...

(defgeneric rescale-component (layout name &rest weights)
  (:documentation
   "Rescale `layout' component with `name' to new `weights'. "))

(defmethod rescale-component ((layout layout-presentation) name &rest weights)
  (check-weights layout weights)
  (with-slots (%components %components-weights) layout
    (if (assoc name %components-weights)
        ;; update component-weights
        (setf (cdr (assoc name %components-weights)) weights)
        (error (format nil "~a does not have component with name ~a"
                       layout name)))))

;; ========== loop-components ==========
;; Should use `loop-components' (recommanded).
;; `loop-components-name' should only be used for developing usage...
;; but use `loop-components-with-name'.

(defmacro loop-components-name ((layout comp-name &rest weight-vars) &body body)
  "Loop through the components of `layout'. "
  (with-gensyms (weights components)
    `(loop with ,weights = (slot-value ,layout '%components-weights)
           with ,components = (slot-value ,layout '%components)
           
           for (,comp-name ,@weight-vars) in ,weights           

           do (progn ,@body))))

(defmacro loop-components-with-name
    ((layout name comp-var &rest weight-vars) &body body)
  "Loop through the components of `layout' with name. "
  (with-gensyms (weights components)
    `(loop with ,weights = (slot-value ,layout '%components-weights)
           with ,components = (slot-value ,layout '%components)
           
           for (,name ,@weight-vars) in ,weights
           for ,comp-var = (gethash ,name ,components)

           do (progn ,@body))))

(defmacro loop-components ((layout comp-var &rest weight-vars) &body body)
  "Loop through the components of `layout'. "
  (with-gensyms (comp-name weights components)
    `(loop with ,weights = (slot-value ,layout '%components-weights)
           with ,components = (slot-value ,layout '%components)
           
           for (,comp-name ,@weight-vars) in ,weights
           for ,comp-var = (gethash ,comp-name ,components)

           do (progn ,@body))))

;; ========== set-stream-bounding-box ==========
;; When `layout-presentation' update its stream-bounding-box size,
;; it should resursively update its inner components stream-bounding-box
;; using `rescale-component' methods.

(defmethod set-stream-bounding-box :after
    ((layout layout-presentation) left right bottom top)
  (declare (ignore left right bottom top))
  (loop-components-name (layout name . weights)
    (apply #'rescale-component layout name weights)))

;; ========== stack-layout-presentation ==========

(defclass stack-layout-presentation (layout-presentation)
  ()
  (:documentation
   "That should be stacked presented.

If the layout is stacked presented, the components of the layout
would be draw one by one (the later added component are drawn on top).

The `%components-weights' of `stack-layout-presentation' element
would be like:

  (component-name u-weight v-weight w-weight h-weight)

The `component-name' can be mapped to component in `%components' as key;
The `u-weight', `v-weight' is the stacked position for the compoent;
The `w-weight', `h-weight' is the stacked size for the compoent. "))

(declaim-weights stack-layout-presentation 4)

;; ========== add-component ==========
;; Could be able to use 0, 2, 4 weights.

(defmethod add-component :around
    ((stack stack-layout-presentation) name component &rest weights)
  (let ((w-len (length weights)))
    (multiple-value-bind (left right bottom top)
        (stream-bounding-box component)
      (let ((width (stream-box-width stack))
            (height (stream-box-height stack)))
        (ecase w-len
          (0 (call-next-method
              stack name component
              0 0
              (float (/ (- right left) width))
              (float (/ (- bottom top) height))))
          (2 (call-next-method
              stack name component
              (first weights) (second weights)
              (float (/ (- right left) width))
              (float (/ (- bottom top) height))))
          (4 (macrolet ((floatp* (x)
                          `(let ((v ,x))
                             (and (floatp v) (<= 0.0 v 1.0)))))
               (cond ((and (floatp* (third weights))
                           (floatp* (fourth weights)))
                      (call-next-method))
                     (t
                      (call-next-method
                       stack name component
                       (first weights) (second weights)
                       (float (/ (- right left) width))
                       (float (/ (- bottom top) height))))))))))))

;; ========== rescale-component ==========
;; The rescale should relative to top-left.

(defmethod rescale-component :after
    ((stack stack-layout-presentation) name &rest weights)
  (multiple-value-bind (left right bottom top)
      (stream-box stack)
    (destructuring-bind (u-w v-w w-w h-w) weights
      (let* ((component    (get-component stack name))
             (width        (- right left))
             (height       (- bottom top))
             (ul (truncate (+ (* u-w width)  left)))
             (vt (truncate (+ (* v-w height) top)))
             (ur (truncate (+ (* w-w width)  ul)))
             (vb (truncate (+ (* h-w height) vt))))
        (set-stream-bounding-box component ul ur vb vt)))))

;; ========== content-relative-layout-presentation ==========
;; Develop Memo: the `set-stream-bounding-box' will not reset
;; the `%cursor-x' and `%cursor-y' it would use
;; `lazy-rescale-component' to rescale the each components.

(defclass content-relative-layout-presentation (layout-presentation)
  ((%cursor-x :initform 0)
   (%cursor-y :initform 0))
  (:documentation
   "That should be present relative to each content.

The content, which would be remembered in `%cursor-x' and `%cursor-y'.
The content should always be the next position where the new component
should be added.
"))

;; ========== reset-content ==========
;; Normally, this should move to top left.

(defgeneric reset-content (layout)
  (:documentation
   "Reset the `layout' content enviroment. "))

(defmethod reset-content ((layout content-relative-layout-presentation))
  (with-slots (%cursor-x %cursor-y) layout
    (setf %cursor-x 0
          %cursor-y 0)))

;; ========== update-content-info ==========
;; This would update cursor position `%cursor-x'
;; and `%cursor-y' according to `weights' infomation.
;; 
;; Every subclass of `content-relative-layout-presentation'
;; should give their own `update-content-info' method.

(defgeneric update-content-info (layout weights)
  (:documentation
   "Update `layout' content with `weights'. "))

;; ========== lazy-rescale-component ==========
;; Rescale component respect to current cursor infomation.
;; lazy-rescale-component should use only current cursor position.

(defgeneric lazy-rescale-component (layout name &rest weights)
  (:documentation
   "Rescale component with `name' without re-initialize contents.
After `lazy-rescale-component', the content infomation would updated. "))

;; ========== add-component ==========

(defmethod add-component
    ((stream content-relative-layout-presentation) name component &rest weights)
  (check-weights stream weights)
  (with-slots (%components %components-weights) stream
    ;; register components to the end of `%components-weights'
    (setf %components-weights
          (nconc %components-weights (list (cons name weights))))

    ;; add components to the `%components'
    (setf (gethash name %components) component))
  (apply #'lazy-rescale-component stream name weights))

;; ========== set-stream-bounding-box ==========
;; In order to calculate and update content faster, use `lazy-rescale-component',
;; which would not calculate the content `%cursor-x' and `%cursor-y',
;; so be careful with it...

(defmethod set-stream-bounding-box :after
    ((layout content-relative-layout-presentation) left right bottom top)
  (declare (ignore left right bottom top))
  (reset-content layout)
  (loop-components-name (layout name . weights)
    (apply #'lazy-rescale-component layout name weights)
    (update-content-info layout weights))
  (reset-content layout))

;; ========== rescale-component ==========
;; Subclass of `content-relative-layout-presentation'
;; should not define their own `rescale-component' method.
;; 
;; Write own `lazy-rescale-component' instead.

(defmethod rescale-component :after
    ((layout content-relative-layout-presentation) name &rest weights)
  (check-weights layout weights)
  (reset-content layout)
  (loop with c-weights = (slot-value layout '%components-weights)
        with components = (slot-value layout '%components)
        for (prev-name . weights) in c-weights
        if (equal prev-name name)
          return t
        else
          do (update-content-info layout weights))
  (apply #'lazy-rescale-component layout name weights))

;; ========== vertical-layout-presentation ==========

(defclass vertical-layout-presentation (content-relative-layout-presentation)
  ()
  (:documentation
   "That should be presented verticlly.

The `%components-weights' element would be like:

  (component-name weight)

The `weight' is the vertical weight for the component;

  layout height * weight => height of component,
  layout left/right      => left/right of component
"))

(declaim-weights vertical-layout-presentation 1)

(defmethod update-content-info ((vertical vertical-layout-presentation) weights)
  (let ((h-w (first weights)))
    (incf (slot-value vertical '%cursor-y)
          (truncate (* h-w (stream-box-height vertical))))))

(defmethod lazy-rescale-component
    ((vertical vertical-layout-presentation) name &rest weights)
  (multiple-value-bind (left right bottom top)
      (stream-box vertical)
    (with-slots (%cursor-y) vertical
      (let* ((c-top    (+ %cursor-y top))
             (c-bottom (truncate (+ (* (first weights) (- bottom top))
                                    c-top))))
        (set-stream-bounding-box (get-component vertical name)
                                 left right c-bottom c-top)
        (setf %cursor-y c-bottom)))))

;; ========== horizontal-layout-presentation ==========

(defclass horizontal-layout-presentation (content-relative-layout-presentation)
  ()
  (:documentation
   "That should be presented horizontally.

The `%components-weights' element would be like:

  (component-name weight)

The `weight' is the horizontal weight for the component;

  layout width * weight => width of component
"))

(declaim-weights horizontal-layout-presentation 1)

(defmethod update-content-info ((horizontal horizontal-layout-presentation) weights)
  (let ((w-w (first weights)))
    (incf (slot-value horizontal '%cursor-x)
          (truncate (* w-w (stream-box-width horizontal))))))

(defmethod lazy-rescale-component
    ((horizontal horizontal-layout-presentation) name &rest weights)
  (multiple-value-bind (left right bottom top)
      (stream-box horizontal)
    (with-slots (%cursor-x) horizontal
      (let* ((c-left  (+ %cursor-x left))
             (c-right (truncate (+ (* (first weights) (- right left))
                                   c-left))))
        (set-stream-bounding-box (get-component horizontal name)
                                 c-left c-right bottom top)
        (setf %cursor-x c-right)))))

;; ========== flow-layout-presentation ==========

(defclass vertical-flow-layout-presentation
    (content-relative-layout-presentation)
  ()
  (:documentation
   "That should be presented vertically.

The `%components-weights' element would be like:

  (component-name weight)

The `weight' is the height of component.

When using `add-component' and not given component height,
the weight would be the component original height.

When rescaling the `vertical-flow-layout-presentation',
it would rescale all the components in ratio.
"))

(declaim-weights vertical-flow-layout-presentation 1)

;; ========== add-component ==========
;; Ensure the `add-component' getting the correct `weights'

(defmethod add-component :around
    ((flow vertical-flow-layout-presentation) name component &rest weights)
  (let ((height (or (first weights) (stream-bounding-box-height component))))
    (call-next-method flow name component height)
    (update-content-info flow (list height))))

(defmethod update-content-info
    ((flow vertical-flow-layout-presentation) weights)
  (let ((height (first weights)))
    (incf (slot-value flow '%cursor-y) height)))

(defmethod lazy-rescale-component
    ((flow vertical-flow-layout-presentation) name &rest weights)
  (multiple-value-bind (left right bottom top)
      (stream-box flow)
    (with-slots (%cursor-y) flow
      (let ((component (get-component flow name)))
        (let* ((height   (first weights))
               (scale    (float
                          (/ height (stream-bounding-box-height component))))
               (width    (truncate
                          (* scale (stream-bounding-box-width component))))
               (n-right  (+ left width))
               (n-top    (+ %cursor-y top))
               (n-bottom (+ n-top height)))
          (set-stream-bounding-box component left n-right n-bottom n-top)
          (when (or (> n-right right) (> n-bottom bottom))
            (set-stream-box flow
              left (max n-right right) (max n-bottom bottom) top)))))))

;; ========== horizontal-flow-layout-presentation ==========

(defclass horizontal-flow-layout-presentation
    (content-relative-layout-presentation)
  ()
  (:documentation
   "That should be presented vertically.

The `%components-weights' element would be like:

  (component-name weight)

The `weight' is the width of component.

When using `add-component' and not given component width,
the weight would be the component original width.

When rescaling the `horizontal-flow-layout-presentation',
it would rescale all the components in ratio.
"))

(declaim-weights horizontal-flow-layout-presentation 1)

(defmethod add-component :around
    ((flow horizontal-flow-layout-presentation) name component &rest weights)
  (let ((width (or (first weights) (stream-bounding-box-width component))))
    (call-next-method flow name component width)))

(defmethod update-content-info
    ((flow horizontal-flow-layout-presentation) weights)
  (let ((width (first weights)))
    (incf (slot-value flow '%cursor-x) width)))

(defmethod lazy-rescale-component
    ((flow horizontal-flow-layout-presentation) name &rest weights)
  (multiple-value-bind (left right bottom top)
      (stream-box flow)
    (with-slots (%cursor-x) flow
      (let ((component (get-component flow name)))
        (multiple-value-bind (c-left c-right c-bottom c-top)
            (stream-bounding-box component)
          (let* ((width    (first weights))
                 (scale    (/ width (- c-right c-left)))
                 (c-left   (+ left %cursor-x))
                 (c-right  (+ c-left width))
                 (c-bottom (+ c-top (truncate (* scale (- c-bottom c-top))))))
            (set-stream-bounding-box component left c-right c-bottom c-top)
            (when (or (> c-right right) (> c-bottom bottom))
              (set-stream-box flow
                              left (max c-right right) (max c-bottom bottom) top))
            (setf %cursor-x c-right)))))))

;; ========== layout-presentation ==========

(defun get-layout-class (layout)
  "Get the layout class name by giving `layout' keyword.

Return values is the name of the layout presentation type. "
  (case layout
    ((:stack :stacked)
     'stack-layout-presentation)
    ((:flow :vertical-flow)
     'vertical-flow-layout-presentation)
    ((:horizontal-flow)
     'horizontal-flow-layout-presentation)
    ((:vertical :vertically)
     'vertical-layout-presentation)
    ((:horizontal :horizontally)
     'horizontal-layout-presentation)
    (otherwise
     (error (format nil "~a is not a known layout. " layout)))))
