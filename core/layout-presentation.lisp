(in-package :gurafu/core)

;; ========== layout-presentation ==========

(defclass layout-presentation (base-presentation)
  ((%components         :initform (make-hash-table))
   (%components-weights :initform ())
   (%xy-top     :initform -1.0)
   (%xy-bottom  :initform 0.0)
   (%xy-left    :initform 0.0)
   (%xy-right   :initform 1.0))
  (:documentation
   "A simple layout mixin. "))

;; ========== layout-presentation ==========

(defmethod present ((stream layout-presentation) &optional backend)
  (with-slots (%components) stream
    (maphash-values
     (lambda (component) (present component backend))
     %components)))

;; ========== verticl-layout-presentation ==========

(defclass verticl-layout-presentation (layout-presentation)
  ()
  (:documentation
   "That should be presented verticlly. "))

(defmethod set-stream-bounding-box :after
    ((stream verticl-layout-presentation) left right bottom top)
  (with-slots (%components-weights %components) stream
    (multiple-value-bind (width height)
        (stream-size stream)
      (multiple-value-bind (left top)
          (stream-position stream)
        (loop with right = (+ left width)              
              for (name weight) in %components-weights
              for component = (gethash name %components)
              for bottom = (truncate (+ (* weight height) top))
              do (set-stream-bounding-box
                  component left right bottom top)
              do (setf top bottom))))))

;; ========== horizontal-layout-mixin ==========

(defclass horizontal-layout-presentation (layout-presentation)
  ()
  (:documentation
   "That should be presented horizontally. "))

(defmethod set-stream-bounding-box :after
    ((stream horizontal-layout-presentation) left right bottom top)
  (with-slots (%components-weights %components) stream
    (multiple-value-bind (width height)
        (stream-size stream)
      (multiple-value-bind (left top)
          (stream-position stream)
        (loop with bottom = (+ height top)
              for (name weight) in %components-weights
              for component = (gethash name %components)
              for right = (truncate (+ (* weight width) left))
              do (set-stream-bounding-box
                  component left right bottom top)
              do (setf left right))))))





