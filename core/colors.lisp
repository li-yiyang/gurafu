(in-package :gurafu/core)

;; ========== more pre-defined colors ==========
;; TODO:
;; + [ ] write a RGB color reader macro
;; + [ ] add chinese traditional colors

(defun parse-hex-color (hex)
  "Read hex color `XXXXXX' into GURAFU color. "
  (loop for i from 2 upto (length hex) by 2
        collect (float (/ (parse-integer (subseq hex (- i 2) i) :radix 16) 255))))

;; ========== chinese traditional colors ==========
;; You could refer to Wikipedia for a subset of chinese
;; traditional colors I use here. More colors should
;; be introduced in the future.
;; 
;; Ref: https://zh.wikipedia.org/zh/中国传统色彩

(defconstant +绛+
  '(0.36862746 0.011764706 0.07058824)
  "比赤更深的红色.

《说文》曰：“绛，大赤也”。")

(defconstant +赤+
  '(0.7647059 0.15294118 0.16862746)
  "介于绛色与朱色之间的红色.

东汉郑玄对《易经》困卦注有“朱深曰赤”。
《尚书·洪范》曰：“赤者，火色也。”
《说文》曰：“赤，南方色也。”

可知赤亦是形容火与代表南方的颜色。")

(defconstant +朱+
  '()
  "传统上的正红色，最纯正的红色。")

(defconstant +丹+
  '()
  "靓丽的朱色，

《说文》曰：丹，巴越之赤石也。赤石即朱砂，故丹即朱砂色。")

(defconstant +红+
  '()
  "浅红，

《说文》曾释曰：“红，帛赤白色也”。
东汉刘熙著《释名》亦言红为：“白色之似绛者”。
又据清人段玉裁对《说文》中“红”一字的加注：“此今人所谓粉红、桃红也”。

可知传统的红色，即近世的粉红色。")

(defconstant +妃红+
  '()
  "浅红的一种，用以描述上流女眷涂抹的脂粉与汗水混合而形成的颜色，一说是绯匹之色。")
