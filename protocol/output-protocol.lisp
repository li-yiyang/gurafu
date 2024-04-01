(in-package :gurafu/protocol)

;; ========== output-protocol ==========

(defclass output-protocol (colored-mixin
                           base-protocol)
  ()
  (:documentation
   "The foundamental protocol class difines the output methods.
It should provide a low level interface to each output media.
"))

;; ========== output-p? ==========

(defrequired output-p? (stream)
  (:documentation
   "Test if a `stream' is a output-able. "))

;; by default, no stream is output-able
(defmethod output-p? (stream)
  (declare (ignore stream))
  nil)

;; the output-protocol stream is output-able
(defmethod output-p? ((stream output-protocol))
  (declare (ignore stream))
  t)

;; ========== output! ==========

(defrequired output! (stream media &key)
  (:documentation
   "Output the `stream' to specific `media'.
The `media' could be a:
+ output path: to render `stream' out to the corresponding file"))

;; ========== draw-point! ==========

(defrequired draw-point! (stream
                          u v
                          &key color pen-width
                          &allow-other-keys)
  (:documentation
   "Draw point on `stream' by absolute position. "))

;; ========== draw-line! ==========

(defrequired draw-line! (stream
                         u1 v1 u2 v2
                         &key color pen-width
                         &allow-other-keys)
  (:documentation
   "Draw line on `stream' by absolute position. "))

;; ========== draw-rect! ==========

(defrequired draw-rect! (stream
                         u1 v1 u2 v2
                         &key color pen-width
                         (fill? t) fill-color
                         &allow-other-keys)
  (:documentation
   "Draw rectangle on `stream' by absolute position.
Default filling (`fill?') with `fill-color'. "))

;; ========== draw-tringle! ==========

(defrequired draw-tringle! (stream
                            u1 v1 u2 v2 u3 v3
                            &key color pen-width
                            (fill? t) fill-color
                            &allow-other-keys)
  (:documentation
   "Draw tringle on `stream' by absolute position.
Default filling (`fill?') with `fill-color'. "))

;; ========== draw-circle! ==========

(defrequired draw-circle! (stream
                           u v uv-r
                           &key color pen-width
                           (fill? t) fill-color
                           &allow-other-keys)
  (:documentation
   "Draw circle on `stream' by absolute position and radius.
Default filling (`fill?') with `fill-color'. "))

;; ========== draw-text! ==========

(defrequired darw-text! (stream
                         u v text
                         &key color font-size font-align
                         font-family font-style
                         rotation &allow-other-keys)
  (:documentation
   "Draw `text' on `stream' by absolute position. "))
