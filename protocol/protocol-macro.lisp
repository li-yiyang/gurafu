(in-package :gurafu/protocol)

(defmacro defrequired (protocol lambda-list &body options)
  (flet ((generic-lambda-list (lambda-list)
           (loop for sym in lambda-list
                 collect (if (listp sym) (first sym) sym)))
         (flat-lambda-list (lambda-list)
           (loop for sym in lambda-list
                 if (not (find sym '(&optional &rest
                                     &key &allow-other-keys)))
                   collect (if (listp sym) (first sym) sym))))
    `(progn
       (defgeneric ,protocol ,(generic-lambda-list lambda-list)
         ,@options)
       (defmethod ,protocol ,lambda-list
         (declare (ignore ,@(flat-lambda-list lambda-list)))
         (error ,(format nil "`~a' not implemented." protocol))))))
