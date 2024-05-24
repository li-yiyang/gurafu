(in-package :gurafu)

;; ========== trivial-indent for GURAFU ==========
;; Why this part?
;; Because I want a better experience (nice indention) for
;; writing GURAFU code, this may also serve as a quick ref
;; for most commonly used functions and a quick introduce
;; of GURAFU.

;; ========== with-present-to-file ==========

(define-indentation with-present-to-file
    ((&whole 4 0 2 &body) ;; present parameters: (var type . args)
     (&whole 4 0 &body)   ;; output file parameters
     &body))              ;; body to draw present

;; ========== add-plot-decorator ==========

(define-indentation add-plot-decorator (&lambda &body))

;; ========== add-plot-data ==========

(define-indentation add-plot-data (4 (&whole 2 0 4 &body) &body))

;; ========== set-stream-box ==========

(define-indentation set-stream-box (4 &body))

;; ========== set-stream-bounding-box ==========

(define-indentation set-stream-bounding-box (4 &body))
