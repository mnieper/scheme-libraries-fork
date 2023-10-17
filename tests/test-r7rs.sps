#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rename (rnrs)
    (syntax-rules r6rs:syntax-rules))
  (scheme-libraries r7rs)
  (scheme-libraries testing))

(test-begin "r7rs")

(test-equal '(2 3)
  ((syntax-rules ()
     [(foo a b)
      (syntax->datum #'(a b))])
   #'(a 2 3)))

(test-equal '(2 3 4)
  ((syntax-rules (_)
     [(foo a b)
      (syntax->datum #'(a b 4))])
   #'(a 2 3)))

(test-equal '(c 3 4)
  ((syntax-rules (_)
     [(foo a b)
      (syntax->datum #'(a b 4))]
     [(foo _ b)
      (syntax->datum #'(c b 4))])
   #'(a 2 3)))


(test-end "r7rs")
