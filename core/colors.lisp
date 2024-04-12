(in-package :gurafu/core)

;; ========== more pre-defined colors ==========
;; TODO:
;; + [ ] write a RGB color reader macro
;; + [ ] add chinese traditional colors

(defun parse-hex-color (hex)
  "Read hex color `XXXXXX' into GURAFU color. "
  (loop for i from 2 upto (length hex) by 2
        collect (float (/ (parse-integer (subseq hex (- i 2) i) :radix 16) 255))))

;; ========== linear-color-map ==========

(defmacro linear-color-map (&rest colors)
  "Make a linear color map function.
Return a lambda function to map unit weight (0 to 1) to colors. "
  (loop with weight = 0.0
        with w = (float (/ 1.0 (1- (length colors))))
        with c-vars = (loop for i below (length colors)
                            collect (gensym "C"))
        
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

;; ========== chinese traditional colors ==========
;; You could refer to Wikipedia for a subset of chinese
;; traditional colors I use here. More colors should
;; be introduced in the future.
;; 
;; Ref: https://zh.wikipedia.org/zh/中国传统色彩

(defconstant +绛+
  '(0.36862746 0.011764706 0.07058824)
  "比赤更深的红色.

《说文》曰: “绛, 大赤也”. ")

(defconstant +赤+
  '(0.7647059 0.15294118 0.16862746)
  "介于绛色与朱色之间的红色.

 东汉郑玄对《易经》困卦注有“朱深曰赤”.
《尚书·洪范》曰: “赤者, 火色也. ”
《说文》曰: “赤, 南方色也. ”
可知赤亦是形容火与代表南方的颜色. ")

(defconstant +朱+
  '(1.0 0.0 0.0)
  "传统上的正红色, 最纯正的红色. ")

(defconstant +丹+
  '(1.0 0.29803923 0.0)
  "靓丽的朱色.

《说文》曰: 丹, 巴越之赤石也. 赤石即朱砂, 故丹即朱砂色. ")

(defconstant +红+
  '(1.0 0.7019608 0.654902)
  "浅红.

《说文》曾释曰: “红, 帛赤白色也”.
东汉刘熙著《释名》亦言红为: “白色之似绛者”.
又据清人段玉裁对《说文》中“红”一字的加注: “此今人所谓粉红、桃红也”.
可知传统的红色, 即近世的粉红色. ")

(defconstant +妃红+
  '(0.92941177 0.34117648 0.21176471)
  "浅红的一种, 用以描述上流女眷涂抹的脂粉与汗水混合而形成的颜色.

一说是绯匹之色. ")

(defconstant +品红+
  '(0.9411765 0.0 0.3372549)
  "略浅于大红的颜色, 用以形容观赏性植物一品红的叶子的颜色. ")

(defconstant +海棠+
  '(0.85882354 0.3529412 0.41960785)
  "传统上对海棠花颜色的形容.
指苹果属梨花海棠的色泽, 呈较桃红色深一些的淡紫红色, 是非常妩媚娇艳的颜色. ")

(defconstant +胭脂+
  '(0.5882353 0.0 0.09411765)
  "女子装扮时所用胭脂的颜色, 亦作国画暗红色颜料的颜色. ")

(defconstant +绯+
  '(0.78431374 0.23529412 0.13725491)
  "深红的一种.

徐铉校补《说文》曰: “绯, 帛赤色也”. ")

(defconstant +赭+
  '(0.6117647 0.3254902 0.2)
  "朱褐混合的颜色.

《说文》曰: “赭, 赤土也”. ")

(defconstant +茜+
  '(0.69411767 0.20784314 0.27450982)
  "从茜草中提取的大红色. ")

(defconstant +殷红+
  '(0.69411767 0.20784314 0.27450982)
  "类似荔枝皮的色泽, 用以描述发黑的红色. ")

(defconstant +桃红+
  '(0.95686275 0.4745098 0.5137255)
  "桃花呈现的淡红, 比粉红略鲜润的颜色. ")
