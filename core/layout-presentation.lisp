(in-package :gurafu/core)

;; ========== layout-presentation ==========

(defclass layout-presentation (base-presentation)
  ((%components         :initform (make-hash-table))
   (%components-weights :initform ()))
  (:documentation
   "A simple layout mixin.

A `layout-presentation' will automatically slove screen coordination
relationships. So those directly inherit `layout-presentation'
should hook after `set-stream-bounding-box' to update the
`%components' coordinates.

For `present' method, every time presenting the component of a
`layout-presentation', it would iter through the `%components',
and apply `present' on each component. "))

;; ========== layout-presentation ==========

(defmethod present ((stream layout-presentation))
  (maphash-values #'present (slot-value stream '%components)))

;; ========== add-component ==========

(defgeneric add-component (stream name component &rest weights)
  (:documentation
   "Add `component' to `stream' as `name' with `weights'.
The component will be append to the end of `%components-weights'. "))

(defmethod add-component
    ((stream layout-presentation) name component &rest weights)
  (with-slots (%components %components-weights) stream
    ;; register components to the end of `%components-weights'
    (setf %components-weights
          (nreverse (cons (cons name weights)
                          (nreverse %components-weights))))
    ;; add components to the `%components'
    (setf (gethash name %components) component)))

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

(defmethod add-component :before
    ((stream stack-layout-presentation) name component &rest weights)
  (declare (ignore name component))
  (unless (= (length weights) 4)
    (error (format nil "Stacked ~a needs 4 weights, got ~a only. "
                   stream weights))))

(defmethod set-stream-bounding-box :after
    ((stream stack-layout-presentation) lfet right bottom top)
  (with-slots (%components-weights %components) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (loop with width  = (- right left)
            with height = (- bottom top)

            for (name u-w v-w w-w h-w) in %components-weights
            for component = (gethash name %components)

            ;; compute the four corner for the component
            for ul = (truncate (+ (* u-w width)  left))
            for vt = (truncate (+ (* v-w height) top))
            for ur = (truncate (+ (* w-w width)  ul))
            for vb = (truncate (+ (* h-w height) vt))

            ;; update inside component position information
            do (set-stream-bounding-box component ul ur vb vt)))))

;; ========== verticl-layout-presentation ==========

(defclass vertical-layout-presentation (layout-presentation)
  ()
  (:documentation
   "That should be presented verticlly.

The `%components-weights' element would be like:

  (component-name weight)

The `component-name' can be mapped to component in `%components' as key;
The `weight' is the vertical weight for the component. "))

(defmethod add-component :before
    ((stream vertical-layout-presentation) name component &rest weights)
  (declare (ignore name component))
  (unless (= (length weights) 1)
    (error (format nil "Vertical ~a needs 1 weights, got ~a only. "
                   stream weights))))

(defmethod set-stream-bounding-box :after
    ((stream vertical-layout-presentation) left right bottom top)
  (with-slots (%components-weights %components) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (loop with height = (- bottom top)
            with vt = top

            for (name weight) in %components-weights
            for component = (gethash name %components)

            ;; compute the position for the inner component
            for vb = (truncate (+ vt (* weight height)))

            do (set-stream-bounding-box component left right vb vt)
            do (setf vt vb)))))

;; ========== horizontal-layout-mixin ==========

(defclass horizontal-layout-presentation (layout-presentation)
  ()
  (:documentation
   "That should be presented horizontally. "))

(defmethod add-component :before
    ((stream horizontal-layout-presentation) name component &rest weights)
  (declare (ignore name component))
  (unless (= (length weights) 1)
    (error (format nil "Horizontal ~a needs 2 weights, got ~a only. "
                   stream weights))))

(defmethod set-stream-bounding-box :after
    ((stream horizontal-layout-presentation) left right bottom top)
  (with-slots (%components-weights %components) stream
    (multiple-value-bind (left right bottom top)
        (stream-box stream)
      (loop with width = (- right left)
            with ul = left

            for (name weight) in %components-weights
            for component = (gethash name %components)

            for ur = (truncate (+ ul (* weight width)))

            do (set-stream-bounding-box component ul ur bottom top)
            do (setf ul ur)))))

;; ========== layout-presentation ==========

(defun get-layout-class (layout)
  "Get the layout class name by giving `layout' keyword.

Return values is the name of the layout presentation type. "
  (case layout
    ((:stack :stacked)
     'stack-layout-presentation)
    ((:flow :vertical-flow :vertical :vertically)
     'vertical-layout-presentation)
    ((:horizontal-flow :horizontal :horizontally)
     'horizontal-layout-presentation)
    (otherwise
     (error (format nil "~a is not a known layout. " layout)))))
