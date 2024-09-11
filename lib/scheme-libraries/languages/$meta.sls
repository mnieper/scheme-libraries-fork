#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

;;; Internal library

(library (scheme-libraries languages $meta)
  (export
    terminals
    make-language-transformer
    parse-language-clauses
    $language->datum)
  (import
    (rnrs)
    (scheme-libraries define-auxiliary-syntax)
    (scheme-libraries multiple-values))

  ;; Languages

  (define-record-type language
    (nongenerative language-a3c7502b-9fe1-455c-81cb-6e7143cbd40e)
    (sealed #t)
    (fields symbols terminals))

  (define-record-type terminal
    (nongenerative terminal-dba3bee3-8440-48f0-b941-7df287189684)
    (sealed #t)
    (fields predicate))

  (define-record-type meta-var
    (nongenerative meta-var-f90ecdca-a538-429d-adb5-7b560a6b2f73)
    (sealed #t)
    (fields type))

  (define language->syntax
    (lambda (lang)
      (define-values (symbols bindings)
        (hashtable-entries (language-symbols lang)))
      (define terminal*
        (vector-fold-right (lambda (i sym bdg terminals)
                             (cons-if (terminal? bdg) sym terminals))
                           symbols bindings))
      (with-syntax ([(terminal ...) (terminal* ...)])
        #'(((terminal ()) ...)))))

  (define-auxiliary-syntax terminals)

  (define-auxiliary-syntax $language->datum)

  (define make-language-transformer
    (lambda (who name language)
      (with-syntax ([name name]
                    [language language])
        (lambda (stx)
          (syntax-case stx ($language->datum)
            [(_ $language->datum)
             (with-syntax ([e #'(define-language name)])
               #''e)]
            [_
             (syntax-violation who "invalid use of language" stx)])))))

  (define parse-language-clauses
    (lambda (who stx clause*)
      (define-values-map (terminal-clause* nonterminal-clause*)
        (lambda (clause) (parse-language-clause who stx clause)) clause*)
      (define-values-map (terminal-symbol* terminal-meta-var**)
        (map (clause) (parse-terminal-clause who stx clause terminal-clause*)))
      (define terminal-predicate
        (map (lambda (symbol) (construct-name symbol symbol "?")) terminal-symbol*))
      (with-syntax ([(terminal-symbol ...) terminal-symbol*]
                    [(terminal-predicate ...) terminal-predicate*]
                    [((terminal-meta-var ...) ...) terminal-meta-var**])
        #'((terminal-symbol ...))

        )


      ))

  (define parse-language-clause
    (lambda (who stx clause)
      (syntax-case stx (terminals)
        [(terminals terminal-clause ...)
         (values #'(terminal-clause ...) '())]
        [nonterminal-clause
         (values '() #'(nonterminal-clause))])))

  (define parse-terminal-clause
    (lambda (who stx clause)
      (syntax-case stx ()
        [(symbol (meta-var ...))
         (for-all identifier? #'(symbol meta-var ...))
         (values #'symbol #'(meta-var ...))]
        [_ (syntax-violation who "invalid terminal clause" stx clause)])))
  )


(receive-map (x* y*) (<proc> ...)
  <body>)

(define-values-map (x* y*) <proc> ... ..)
