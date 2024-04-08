(in-package :gurafu/core)

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

;; ========== *foreground-color* ==========
;; ========== *background-color* ==========

(defparameter *foreground-color* +black+
  "The default GURAFU color draw foreground. ")

(defparameter *background-color* +white+
  "The default GURAFU color draw background. ")

;; ========== more pre-defined colors ==========
;; TODO:
;; + [ ] write a RGB color reader macro
;; + [ ] add chinese traditional colors

