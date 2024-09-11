#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries languages)
  (export define-language
          language->datum
          terminals)
  (import (rnrs)
          (scheme-libraries define-who)
          (scheme-libraries languages $meta))

  (define-syntax/who define-language
    (lambda (stx)
      (syntax-case stx ()
        [(_ language-name clause ...)
         (identifier? #'language-name)
         (with-syntax ([language (parse-language-clauses who stx #'(clause ...))])
           #'(define-syntax/who language-name (make-language-transformer who #'language-name #'language)))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who language->datum
    (lambda (stx)
      (syntax-case stx ()
        [(_ language-name)
         (identifier? #'language-name)
         #'(language-name $language->datum)]
        [_ (syntax-violation who "invalid syntax" stx)])))
  )
