(in-package :gurafu/core)

;; ========== parse-presentation-superclasses ==========

(defun parse-presentation-superclasses (direct-superclass options)
  "This would determine the direct-superclass of object. "
  (let* ((layout (second (assoc :layout options)))
         (class (if layout (get-layout-class layout) 'base-presentation)))
    (if (find class direct-superclass)
        direct-superclass
        (if (null direct-superclass)
            (list class)
            direct-superclass))))

;; ========== parse-presentation-draw-options ==========

(defun parse-presentation-draw-options (class options)
  "Defines how the `present' is drawed. "
  (let ((draw (cdr (assoc :draw options))))
    (when draw
      (destructuring-bind (slot-vars . codes) draw
        (let ((present (intern "SELF")))
          `(defmethod present :after ((,present ,class))
             (with-slots ,slot-vars ,present
               ,@codes)))))))

;; ========== parse-layout-bind ==========

(flet ((stack-weighted (description)
         (loop for item in description
               if (listp item)
                 collect item
               else
                 collect (list item 0.0 0.0 1.0 1.0)))
       
       (non-stack-weighted (description)
         (let ((weighted ())
               (not-weighted ())
               (switches ())
               (remain 1.0))
           ;; collect all the weighted and not-weighted
           (loop for item in description do
             (cond ((listp item)
                    (push item weighted)
                    (push nil switches)
                    (decf remain (second item)))
                   (t
                    (push item not-weighted)
                    (push t switches))))
           ;; if existing not-weighted element, give them evenly weigts
           (loop with weight = (/ remain (if not-weighted (length not-weighted) 1))
                 with res = ()
                 while switches do
                   (if (pop switches)
                       ;; switch means to assign item with new weight
                       (push (list (pop not-weighted) weight) res)
                       (push (pop weighted) res))
                 finally (return res)))))
  
  (defun generate-layout-bind (name layout)
    (loop for (component . weights) in (case (first layout)
                                         ((:stack :stacked)
                                          (stack-weighted (rest layout)))
                                         (otherwise
                                          (non-stack-weighted (rest layout))))
          with comp-name = (gensym)
          if (listp component)
            collect `(let ((,comp-name (make-instance ',(get-layout-class
                                                         (first component)))))
                       ,@(generate-layout-bind comp-name component)
                       (add-component ,name ',component ,comp-name ,@weights))        
            and do (setf comp-name (gensym))
          else
            collect `(add-component ,name ',component ,component ,@weights))))

(defun generate-component-init (description)
  "The `description' element is like:

     (name component-type &rest init-args)

   Return value are list with element like:

     (name (make-instance 'component-type . init-args))"
  (loop for (name type . init-args) in description
        collect `(,name (make-instance ',type ,@init-args))))

(defun parse-presentation-init-options (class options)
  "Parse the `options' for the presentation.

The `options' could have both:

  (:components (slot-value)
    components-definitions) truned into initialize-instance :after method let binding
  (:layout ...)             truned into components let code body

Or the `options' should not have them all. "
  (let ((components (rest (assoc :components options)))
        (layout     (rest (assoc :layout options))))
    (cond ((and components (not layout))
           (error "Providing components but missing layout. "))
          ((and layout (not components))
           (error "Providing layout but missing components. "))
          ((and components layout)
            (with-gensyms (present)
              `(defmethod initialize-instance :after
                   ((,present ,class) &key)
                 (with-slots ,(first components) ,present
                   (let ,(generate-component-init (rest components))
                     ,@(generate-layout-bind present layout)))))))))

;; ========== define-presentation ==========

(defmacro define-presentation (name direct-superclass slots &rest options)
  "Feeling happy with higher level of defining a presentation object class.

  (define-presentation name (. direct-superclass)
    (... slots)
    ;; this is low-level about how to draw a presentation object
    ;; please NOTE that within the defmethod of draw, object should be refered as
    ;; `self' (may change in the future)
    (:draw (slot-var-names)
      (program-to-draw-object))

    ;; this is higher-level about how to layout a presentation object
    (:components (slot-var-names)
      (component-name component-type . component-init-args))
    (:layout :vertical|:horizontal|:stack
      (component-name weight) ; specify the component weight
      component-name          ; the weight will be automatically calculated
      ))"
  `(progn
     (defclass ,name ,(parse-presentation-superclasses
                       direct-superclass options)
       ,slots)
     ,(parse-presentation-draw-options name options)
     ,(parse-presentation-init-options name options)))
