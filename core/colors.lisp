(in-package :gurafu/core)

;; ========== more pre-defined colors ==========
;; TODO:
;; + [ ] write a RGB color reader macro
;; + [ ] add chinese traditional colors

;; ========== color reader ==========

(defun parse-hex-color (hex)
  "Read hex color `XXXXXX' into GURAFU color. "
  (loop for i from 2 upto (length hex) by 2
        collect (float (/ (parse-integer (subseq hex (- i 2) i) :radix 16) 255))))

;; ========== colorspace converter ==========
;; The following part refers to 色空間の変換 (Transformation
;; of Colorspace) by T.Fujiwara 2012/01.
;; 
;; Ref:
;; + 色空間の変換 https://fujiwaratko.sakura.ne.jp/infosci/colorspace/index.html
;;   WebArchive:  https://web.archive.org/web/20240412163239/https://fujiwaratko.sakura.ne.jp/infosci/colorspace/index.html

;; ========== color convert constants ==========

(defparameter +xyz-standard-illuminant-E+
  '(1.0 1.0 1.0)
  "XYZ standard illuminant white when distributed with same energy (5454K).
Used in `lab-to-xyz-color' functions for Lab to XYZ colorspace. ")

(defparameter +xyz-standard-illuminant-D50+
  '(0.9642	1.0	0.8249)
  "XYZ standard illuminant white for natural light (5000K).
Used in `lab-to-xyz-color' functions for Lab to XYZ colorspace. ")

(defparameter +xyz-standard-illuminant-D65+
  '(0.95046	1.0	1.08906)
  "XYZ standard illuminant white for natural light (6500K).
Used in `lab-to-xyz-color' functions for Lab to XYZ colorspace. ")

(defparameter +xyz-standard-illuminant-C+
  '(0.98071	1.0	1.18225)
  "XYZ standard illuminant white for natural light (old standard 6774K).
Used in `lab-to-xyz-color' functions for Lab to XYZ colorspace. ")

(defparameter +srgb-d65-to-xyz-d65-matrix+
  '((0.412391  0.357584  0.180481)
    (0.212639  0.715169  0.072192)
    (0.019331  0.119195  0.950532))
  "sRGB (D65) to XYZ (D65) matrix")

(defparameter +p3-d65-to-xyz-d65-matrix+
  '((0.486571  0.265668  0.198217)
    (0.228975  0.691739  0.079287)
    (0.000000  0.045113  1.043944))
  "P3-D65 (D65) to XYZ (D65) matrix")

(defparameter +adobe-rgb-d65-to-xyz-d65-matrix+
  '((0.576669  0.185558  0.188229)
    (0.297345  0.627364  0.075291)
    (0.027031  0.070689  0.991338))
  "Adobe RGB (D65) to XYZ (D65) matrix")

(defparameter +ntsc-rgb-c-to-xyz-c-matrix+
  '((0.6069    0.1735    0.2003)
    (0.2989    0.5866    0.1145)
    (0.0000    0.0661    1.1162))
  "NTSC RGB (C) to XYZ (D65) matrix")

(defparameter +srgb-d65-to-xyz-d50-matrix+
  '((0.436041  0.385113  0.143046)
    (0.222485  0.716905  0.060610)
    (0.013920  0.097067  0.713913))
  "sRGB (D65) to XYZ (D50) matrix")

(defparameter +p3-d65-to-xyz-d50-matrix+
  '(( 0.515119  0.291978  0.157103)
    ( 0.241189  0.692244  0.066567)
    (-0.001050  0.041879  0.784071))
  "P3-D65 (D65) to XYZ (D50) matrix")

(defparameter +adobe-rgb-d65-to-xyz-d50-matrix+
  '(( 0.609741  0.205273  0.149187)
    ( 0.311113  0.625675  0.063212)
    ( 0.019465  0.060874  0.744560))
  "Adobe RGB (D65) to XYZ (D50) matrix")

(defparameter +xyz-d65-to-srgb-d65-matrix+
  '(( 3.240970 -1.537383 -0.498611)
    (-0.969244  1.875968  0.041555)    
    ( 0.055630 -0.203977  1.056972))
  "XYZ (D65) to sRGB (D65) matrix")

(defparameter +xyz-d65-to-p3-d65-matrix+
  '(( 2.493497 -0.931384 -0.402711)
    (-0.829489  1.762664  0.023625)
    ( 0.035846 -0.076172  0.956885))
  "XYZ (D65) to P3-D65 (D65) matrix")

(defparameter +xyz-d65-to-adobe-rgb-d65-matrix+
  '(( 2.041588 -0.565007 -0.344731)
    (-0.969244  1.875968  0.041555)
    ( 0.013444 -0.118362  1.015175))
  "XYZ (D65) to Adobe RGB (D65) matrix")

(defparameter +xyz-c-to-ntsc-rgb-c-matrix+
  '(( 1.9100   -0.5325   -0.2882)
    (-0.9846    1.9991   -0.0283)
    ( 0.0583   -0.1184    0.8976))
  "XYZ (D65) to NTSC RGB (C) matrix")

(defparameter +xyz-d50-to-srgb-d65-matrix+
  '(( 3.134187 -1.617209 -0.490694)
    (-0.978749  1.916130  0.033433)
    ( 0.071964 -0.228994  1.405754))
  "XYZ (D50) to RGB (D65) matrix")

(defparameter +xyz-d50-to-p3-d65-matrix+
  '(( 2.403984 -0.989907 -0.397642)
    (-0.842223  1.798844  0.016035)
    ( 0.048206 -0.097407  1.274005))
  "XYZ (D50) to P3-D65 (D65) matrix")

(defparameter +xyz-d50-to-adobe-rgb-d65-matrix+
  '(( 1.962517 -0.610651 -0.341384)
    (-0.978749  1.916130  0.033433)
    ( 0.028715 -0.140696  1.349266))
  "XYZ (D50) to Adobe RGB (D65) matrix")

;; ========== color convert helper functions ==========

(declaim (inline matrix-product rgb-xyz-matrix xyz-rgb-matrix))
(defun matrix-product (matrix vec)
  (mapcar (lambda (v) (apply #'+ (mapcar #'* v vec))) matrix))

(defun rgb-xyz-matrix (&optional (rgb :srgb) (xyz :D65))
  "Get the RGB to XYZ transfer matrix by given colorspace. "
  (ecase xyz
    (:D50 (ecase rgb
            (:srgb      +srgb-d65-to-xyz-d50-matrix+)
            (:p3-d65    +p3-d65-to-xyz-d50-matrix+)
            (:adobe-rgb +adobe-rgb-d65-to-xyz-d50-matrix+)))
    (:D65 (ecase rgb
            (:srgb      +srgb-d65-to-xyz-d65-matrix+)
            (:p3-d65    +p3-d65-to-xyz-d65-matrix+)
            (:adobe-rgb +adobe-rgb-d65-to-xyz-d65-matrix+)))
    (:C   (ecase rgb
            (:ntsc      +ntsc-rgb-c-to-xyz-c-matrix+)))))

(defun xyz-rgb-matrix (&optional (xyz :D65) (rgb :srgb))
  "Get the XYZ to RGB transfer matrix by given colorspace. "
  (ecase xyz
    (:D50 (ecase rgb
            (:srgb      +xyz-d50-to-srgb-d65-matrix+)
            (:p3-d65    +xyz-d50-to-p3-d65-matrix+)
            (:adobe-rgb +xyz-d50-to-adobe-rgb-d65-matrix+)))
    (:D65 (ecase rgb
            (:srgb      +xyz-d65-to-srgb-d65-matrix+)
            (:p3-d65    +xyz-d65-to-p3-d65-matrix+)
            (:adobe-rgb +xyz-d65-to-adobe-rgb-d65-matrix+)))
    (:C   (ecase rgb
            (:ntsc      +xyz-c-to-ntsc-rgb-c-matrix+)))))

;; ========== *-to-*-color color converter ==========

(defun rgb-to-xyz-color (rgb &optional (rgb-space :srgb) (xyz-space :D65))
  "Trun RGB colorspace to XYZ colorspace. "
  (matrix-product (rgb-xyz-matrix rgb-space xyz-space) rgb))

(defun xyz-to-rgb-color (xyz &optional (rgb-space :srgb) (xyz-space :D65))
  "Trun XYZ colorspace to RGB colorspace. "
  (matrix-product (xyz-rgb-matrix xyz-space rgb-space) xyz))

(defun xyz-to-lab-color (xyz &optional (xyz-standard-white :D50))
  "Trun XYZ (D50) to LAB color.

  F(t) = 116 t^{1/3} - 16 if t > (6/29)^3
    else (29/3)^3 t

  L* = F(Y/Yw)
  a* = (500/116) [F(X/Xw) - f(Y/Yw)]
  b* = (200/116) [F(Y/Yw) - f(Z/Zw)]
"
  (destructuring-bind (fx fy fz)
      (mapcar (lambda (v)
                (if (> v (expt (/ 6 29) 3))
                    (- (* 116 (expt v 1/3)) 16)
                    (* (expt (/ 29 3) 3) v)))
              (mapcar #'/ xyz (ecase xyz-standard-white
                                (:E   +xyz-standard-illuminant-e+)
                                (:D50 +xyz-standard-illuminant-d50+)
                                (:D65 +xyz-standard-illuminant-d65+)
                                (:C   +xyz-standard-illuminant-c+))))
    (list fy
          (* (/ 500.0 116.0) (- fx fy))
          (* (/ 200.0 116.0) (- fy fz)))))

(defun lab-to-xyz-color (lab &optional (xyz-standard-white :D50))
  "Convert a CIELab color to XYZ color. "
  (destructuring-bind (l a b) lab
    (mapcar (lambda (f w)
              (if (> f (/ 6.0 29.0))
                  (* (expt f 3) w)
                  (* (expt (/ 3.0 29.0) 3) (- (* 116.0 f) 16) w)))
            (let ((fy (/ (+ l 16.0) 116.0)))
              (list (+ fy (/ a 500.0)) fy (- fy (/ b 200.0))))
            (ecase xyz-standard-white
              (:E   +xyz-standard-illuminant-e+)
              (:D50 +xyz-standard-illuminant-d50+)
              (:D65 +xyz-standard-illuminant-d65+)
              (:C   +xyz-standard-illuminant-c+)))))

(defun lab-to-rgb-color (lab &optional (xyz-standard-white :D50)
                               (rgb-space :srgb))
  "Convert a CIELab color to RGB color. "
  (xyz-to-rgb-color (lab-to-xyz-color lab xyz-standard-white)
                    rgb-space :D50))

;; ========== linear-color-map ==========

(defmacro linear-color-map (color1 color2 &rest more-colors)
  "Make a linear color map function. At least two colors should provided. 
Return a lambda function to map unit weight (0 to 1) to colors. "
  (loop with weight = 0.0
        with colors = (cons color1 (cons color2 more-colors))
        with size   = (length colors)
        with w      = (float (/ 1.0 (1- size)))
        with c-vars = (loop for i below size collect (gensym "C"))
        
        for (c1 c2) on c-vars
        while c2
        
        collect `((< w ,(incf weight w)) (+ (* (- 1 w) ,c1) (* w ,c2)))
          into condition

        finally (return `(lambda (w)
                           (let ((w (min 1.0 (max w 0.0))))
                             (mapcar (lambda ,c-vars
                                       (cond ((< w 0) ,(first c-vars))
                                             ,@condition
                                             (t ,(car (last c-vars)))))
                                     ,@colors))))))

;; ========== more color names ==========

;; ================= 《天工开物》 (The Tiangong Kaiwu) 色名 ===================
;; ========== Color Names in The Exploitation of the Works of Nature ==========
;; The color values in 《天工开物》 was extracted from the book 《染作江南春水色》
;; (The Classical Colors of China), written by 金成熺. The table is extracted
;; from the Table 7-7 (page 147-149 for 1st).

(defparameter +大红+
  (lab-to-rgb-color '(59.19 40.14 12.21)))

(defparameter +莲红+
  (lab-to-rgb-color '(58.67 39.98 12.69)))

(defparameter +桃红+
  (lab-to-rgb-color '(71.82 33.00 6.09)))

(defparameter +银红+
  (lab-to-rgb-color '(77.22 27.75 4.20)))

(defparameter +水红+
  (lab-to-rgb-color '(83.00 20.03 3.33)))

(defparameter +木红+
  (lab-to-rgb-color '(58.58 16.06 15.82)))

(defparameter +鹅黄+
  (lab-to-rgb-color '(77.89 -10.80 20.20)))

(defparameter +紫+
  (lab-to-rgb-color '(31.12 6.82 -2.58)))

(defparameter +天青+
  (lab-to-rgb-color '(43.50 8.53 -7.03)))

(defparameter +葡萄青+
  (lab-to-rgb-color '(30.12 10.62 -4.42)))

(defparameter +蛋青+
  (lab-to-rgb-color '(58.11 -9.79 -12.57)))

(defparameter +翠蓝+
  (lab-to-rgb-color '(57.08 -7.13 -20.54)))

(defparameter +天蓝+
  (lab-to-rgb-color '(61.44 -7.07 -18.82)))

(defparameter +月白+
  (lab-to-rgb-color '(70.66 -6.76 -13.43)))

(defparameter +草白+
  (lab-to-rgb-color '(76.42 -5.51 -9.83)))

(defparameter +毛青+
  (lab-to-rgb-color '(27.21 -0.13 -24.75)))

(defparameter +大红官绿+
  (lab-to-rgb-color '(64.79 -12.25 -2.04)))

(defparameter +豆绿+
  (lab-to-rgb-color '(69.22 -14.49 7.85)))

(defparameter +油绿+
  (lab-to-rgb-color '(58.03 -0.84 16.20)))

(defparameter +藕色+
  (lab-to-rgb-color '(70.73 8.60 10.70)))

(defparameter +茶褐+
  (lab-to-rgb-color '(55.44 29.48 21.97)))

(defparameter +包头青+
  (lab-to-rgb-color '(40.74 0.81 5.46)))
