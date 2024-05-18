(in-package :gurafu)

;; ========== trivial-indent for GURAFU ==========
;; Why this part?
;; Because I want a better experience (nice indention) for
;; writing GURAFU code, this may also serve as a quick ref
;; for most commonly used functions and a quick introduce
;; of GURAFU.

(define-indentation with-present-to-file
    ((&whole 4 0 2 &body) ;; present parameters: (var type . args)
     (&whole 4 0 &body)   ;; output file parameters
     &body))              ;; body to draw present

(define-indentation add-plot-decorator (&lambda &body))

(define-indentation add-plot-data (4 (&whole 2 0 4 &body) &body))
