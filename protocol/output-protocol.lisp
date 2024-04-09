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

;; ========== draw-pixel! ==========

(defrequired draw-pixel! (stream u v color)
  (:documentation
   "Set pixel on `stream'. "))

;; ========== draw-point! ==========

(defrequired draw-point! (stream
                          u v
                          &key color pen-width point-style
                          &allow-other-keys)
  (:documentation
   "Draw point on `stream' by absolute position.

Return values are uv bounding box left, right, bottom and top. "))

;; ========== draw-line! ==========

(defrequired draw-line! (stream
                         u1 v1 u2 v2
                         &key color pen-width line-style
                         &allow-other-keys)
  (:documentation
   "Draw line on `stream' by absolute position.
Return values are uv bounding box left, right, bottom and top.

The `line-style' could be `:solid', `:dashed', `:dash-dot'... "))

;; ========== draw-rect! ==========

(defrequired draw-rect! (stream
                         u1 v1 u2 v2
                         &key color pen-width line-style
                         (fill? t) fill-color
                         &allow-other-keys)
  (:documentation
   "Draw rectangle on `stream' by absolute position.
Default filling (`fill?') with `fill-color'.
Return values are uv bounding box left, right, bottom and top. "))

;; ========== draw-tringle! ==========

(defrequired draw-tringle! (stream
                            u1 v1 u2 v2 u3 v3
                            &key color pen-width line-style
                            (fill? t) fill-color
                            &allow-other-keys)
  (:documentation
   "Draw tringle on `stream' by absolute position.
Default filling (`fill?') with `fill-color'.
Return values are uv bounding box left, right, bottom and top. "))

;; ========== draw-circle! ==========

(defrequired draw-circle! (stream
                           u v uv-r
                           &key color pen-width line-style
                           (fill? t) fill-color
                           &allow-other-keys)
  (:documentation
   "Draw circle on `stream' by absolute position and radius.
Default filling (`fill?') with `fill-color'.
Return values are uv bounding box left, right, bottom and top. "))

;; ========== draw-text-width! ==========

(defrequired draw-text-size! (stream
                              text
                              &key text-path text-align
                              line-width line-spacing
                              char-spacing font-size font-name
                              &allow-other-keys)
  (:documentation
   "Get the size of `text' when drawing on `stream'. "))

;; ========== draw-text! ==========

(defrequired darw-text! (stream
                         u v text
                         &key color text-path text-align
                         line-width line-spacing
                         char-spacing
                         font-size font-name
                         &allow-other-keys)
  (:documentation
   "Draw `text' on `stream' by absolute position.
Return values are bounding uv box left, right, bottom and top coordinates. "))
