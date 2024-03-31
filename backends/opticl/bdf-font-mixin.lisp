(in-package :gurafu/backends/opticl)

(let ((font-table (make-hash-table)))
  ;; Load fonts from fonts directory
  (loop for path in (uiop:directory-files
                     (asdf:system-relative-pathname
                      :gurafu
                      "backends/opticl/fonts/"))
        for font = (make-keyword (string-upcase (pathname-name path)))
        do (setf (gethash font font-table)
                 (load-bdf-font path)))
  (defparameter *opticl-bdf-fonts* font-table
    "Opticl BDF fonts database. "))

(defclass bdf-font-mixin ()
  ()
  (:documentation
   "Provide BDF font mixin functionality. "))

(defgeneric find-font (obj &key font-name font-style &allow-other-keys)
  (:documentation
   "Find font.
Return `bdf' object of font. "))

(defmethod find-font ((obj bdf-font-mixin)
                      &key (font-name "UNIFONT")
                        font-style
                      &allow-other-keys)
  (declare (ignore font-style))
  (let* ((font (make-keyword (string-upcase font-name))))
    (gethash font *opticl-bdf-fonts* nil)))
