(in-package :gurafu/protocol)

;; ========== pre-defined colors ==========

;; ========== basic pre-defined colors ==========
;; Below are some basic colors defined for simple usage.
;; Although I do not recommand you to use this colors,
;; except for `+white+' and `+black+', since they are
;; in high saturation, which is quite ugly, though...

(defconstant +white+
  '(1.0 1.0 1.0)
  "GURAFU white color. ")

(defconstant +black+
  '(0.0 0.0 0.0)
  "GURAFU black color. ")

(defconstant +red+
  '(1.0 0.0 0.0)
  "GURAFU red color. ")

(defconstant +green+
  '(0.0 1.0 0.0)
  "GURAFU green color. ")

(defconstant +blue+
  '(0.0 0.0 1.0)
  "GURAFU blue color. ")

(defconstant +yellow+
  '(1.0 1.0 0.0)
  "GURAFU yellow color. ")

(defconstant +gray+
  '(0.1 0.1 0.1)
  "GURAFU gray color. ")

;; ========== *foreground-color* ==========
;; ========== *background-color* ==========

(defparameter *foreground-color* +black+
  "The default GURAFU color draw foreground. ")

(defparameter *background-color* +white+
  "The default GURAFU color draw background. ")

(defparameter +colorful-colorspace+
  '(:rgb :8-bit-rgb)
  "Colorspaces supporting colored output. ")

(defparameter +grayful-colorspace+
  '(:gray :grayscale :grey :greyscale)
  "Colorspaces supporting grayscale output. ")

;; ========== colored-mixin ==========

(defclass colored-mixin ()
  ((%colorspace :initform :8-bit-rgb
                :initarg :colorspace
                :reader  colorspace!))
  (:documentation
   "This foundamental protocol class defines the color methods.
It should perform as a translation between different device
color settings.
"))

(defmethod initialize-instance :after ((stream colored-mixin) &key)
  ;; Ensure the colorspace is within defination
  (with-slots (%colorspace) stream
    (unless (or (find %colorspace +colorful-colorspace+)
                (find %colorspace +grayful-colorspace+))
      (error (format nil "Undefined colorspace: ~a" %colorspace)))))

;; ========== colorful? ==========

(defrequired colorful? (stream)
  (:documentation
   "Test if a `stream' supports colored output. "))

(defmethod colorful? ((stream colored-mixin))
  (find (slot-value stream '%colorspace) +colorful-colorspace+))

;; ========== real-colorspace-name! ==========

(defrequired real-colorspace-name! (stream)
  (:documentation
   "Get the colorspace name for specific backend."))

;; ========== rgb-color! ==========

(defrequired rgb-color! (stream rgb-color)
  (:documentation
   "Decode the rgb color list `rgb-color' to specific backend color.
The `rgb-color' for example is `(1.0 1.0 1.0)' for white.
The return values should be color vector in each color space,
formatted for each backend. "))
