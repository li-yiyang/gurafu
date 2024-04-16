(in-package :gurafu/core)

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

;; ========== layout-presentation ==========

(defmethod present ((layout layout-presentation))
  (maphash-values #'present (slot-value layout '%components)))

;; ========== get-component ==========

(declaim (inline get-component))

(defgeneric get-component (layout name)
  (:documentation
   "Get the component of `stream' with `name'. "))

(defmethod get-component ((layout layout-presentation) name)
  (gethash name (slot-value layout '%components)))

;; ========== check-weights ==========

(declaim (inline check-weights))

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
The component will be append to the end of `%components-weights'. "))

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

(defmacro loop-components-name ((layout comp-name &rest weight-vars) &body body)
  "Loop through the components of `layout'. "
  (with-gensyms (weights components)
    `(loop with ,weights = (slot-value ,layout '%components-weights)
           with ,components = (slot-value ,layout '%components)
           
           for (,comp-name ,@weight-vars) in ,weights           

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

The `%components-weights' of `stack-layout-presentation' element
would be like:

  (component-name u-weight v-weight w-weight h-weight)

The `component-name' can be mapped to component in `%components' as key;
The `u-weight', `v-weight' is the stacked position for the compoent;
The `w-weight', `h-weight' is the stacked size for the compoent. "))

(declaim-weights stack-layout-presentation 4)

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
;; Develop Memo: currently, the `set-stream-bounding-box' will calculate
;; the `%cursor-x' and `%cursor-y' repeatly, O(n^2) complexity, which
;; might cause unnecessary time waste.
;; 
;; SHOULD FIX IT in the next code clean up.

(defclass content-relative-layout-presentation (layout-presentation)
  ((%cursor-x :initform 0)
   (%cursor-y :initform 0))
  (:documentation
   "That should be present relative to each content.

The content, which would be remembered in `%cursor-x' and `%cursor-y'."))

;; ========== reset-content ==========

(declaim (inline reset-content))

(defgeneric reset-content (layout)
  (:documentation
   "Reset the `layout' content enviroment. "))

(defmethod reset-content ((layout content-relative-layout-presentation))
  (with-slots (%cursor-x %cursor-y) layout
    (setf %cursor-x 0
          %cursor-y 0)))

;; ========== update-content-info ==========

(declaim (inline update-content-info))

(defgeneric update-content-info (layout weights)
  (:documentation
   "Update `layout' content with `weights'. "))

;; ========== lazy-rescale-component ==========

(declaim (inline lazy-rescale-component))

(defgeneric lazy-rescale-component (layout name &rest weights)
  (:documentation
   "Rescale component with `name' without re-initialize contents.
After `lazy-rescale-component', the content infomation would updated. "))

;; ========== set-stream-bounding-box ==========
;; In order to calculate and update content faster, use `lazy-rescale-component',
;; which would not calculate the content `%cursor-x' and `%cursor-y',
;; so be careful with it...

(defmethod set-stream-bounding-box :after
    ((layout content-relative-layout-presentation) left right bottom top)
  (declare (ignore left right bottom top))
  (reset-content layout)
  (loop-components-name (layout name . weights)
    (apply #'lazy-rescale-component layout name weights))
  (reset-content layout))

;; ========== rescale-component ==========

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
   ""))

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
   ""))

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

(defclass flow-layout-presentation (content-relative-layout-presentation
                                    auto-enlarge-backend-mixin)
  ()
  (:documentation
   ""))

(declaim-weights flow-layout-presentation 1)

(defmethod add-component :around
    ((flow flow-layout-presentation) name component &rest weights)
  (let ((height (or (first weights) (stream-bounding-box-height component))))
    (call-next-method flow name component height)))

(defmethod update-content-info ((flow flow-layout-presentation) weights)
  (let ((height (first weights)))
    (incf (slot-value flow '%cursor-y) height)))

(defmethod lazy-rescale-component
    ((flow flow-layout-presentation) name &rest weights)
  (multiple-value-bind (left right bottom top)
      (stream-box flow)
    (with-slots (%cursor-y) flow
      (let ((component (get-component flow name)))
        (multiple-value-bind (c-left c-right c-bottom c-top)
            (stream-bounding-box component)
          (let* ((height   (first weights))
                 (scale    (/ height (- c-bottom c-top)))
                 (c-right  (+ left (truncate (* scale (- c-right c-left)))))
                 (c-top    (+ top %cursor-y))
                 (c-bottom (+ c-top height)))
            (set-stream-bounding-box component left c-right c-bottom c-top)
            (when (or (> c-right right) (> c-bottom bottom))
              (set-stream-box flow
                              left (max c-right right) (max c-bottom bottom) top))
            (setf %cursor-y c-bottom)))))))
