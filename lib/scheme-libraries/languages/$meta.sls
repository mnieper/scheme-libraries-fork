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
    (scheme-libraries define-values)
    (scheme-libraries helpers)
    (scheme-libraries macros)
    (scheme-libraries multiple-values)
    (scheme-libraries vectors))

  ;; Languages

  (define-record-type language
    (nongenerative language-a3c7502b-9fe1-455c-81cb-6e7143cbd40e)
    (sealed #t)
    (fields symbols))

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
                             (cons-if (terminal? bdg) bdg terminals))
                           '() symbols bindings))
      (define terminal-symbol*
        (vector-fold-right (lambda (i sym bdg terminals)
                             (cons-if (terminal? bdg) (datum->syntax #'* sym) terminals))
                           '() symbols bindings))
      (define terminal-predicate* (map terminal-predicate terminal*))
      (define terminal-meta-var**
        (map (lambda (terminal)
               (vector-fold-right (lambda (i sym bdg meta-vars)
                                    (cons-if (and (meta-var? bdg) (eqv? (meta-var-type bdg) terminal))
                                             (datum->syntax #'* sym) meta-vars))
                                  '() symbols bindings))
             terminal*))
      (with-syntax ([(terminal-symbol ...) terminal-symbol*]
                    [(terminal-predicate ...) terminal-predicate*]
                    [((terminal-meta-var ...) ...) terminal-meta-var**])
        #'(((terminal-symbol terminal-predicate (terminal-meta-var ...)) ...)))))

  (define-auxiliary-syntax terminals)

  (define-auxiliary-syntax $language->datum)

  (define make-language-transformer
    (lambda (who name language)
      (with-syntax ([name name]
                    [language language])
        (lambda (stx)
          (syntax-case stx ($language->datum)
            [(_ $language->datum)
             (with-syntax ([(((terminal-symbol terminal-predicate (terminal-meta-var ...)) ...)) #'language])
               #''(define-language name
                    (terminals (terminal-symbol (terminal-meta-var ...)) ...)))]
            [_
             (syntax-violation who "invalid use of language" stx)])))))

  (define parse-language-clauses
    (lambda (who stx clause*)
      (define-values-append-map (terminal-clause* nonterminal-clause*)
        (lambda (cl) (parse-language-clause who stx cl)) clause*)
      (define-values-map (terminal-symbol* terminal-meta-var**)
        (lambda (cl) (parse-terminal-clause who stx cl)) terminal-clause*)
      (define terminal-predicate*
        (map (lambda (symbol) (construct-name symbol symbol "?")) terminal-symbol*))
      (define symbols (make-symbol-hashtable))
      (define symbol-insert!
        (lambda (id bdg)
          (define sym (syntax->datum id))
          (when (hashtable-contains? symbols sym)
            (syntax-violation who "duplicate symbol in language definition" stx id))
          (hashtable-set! symbols sym bdg)))
      (for-each
       (lambda (sym pred mvar*)
         (define terminal (make-terminal pred))
         (symbol-insert! sym terminal)
         (for-each
          (lambda (mvar)
            (symbol-insert! mvar (make-meta-var terminal)))
          mvar*))
       terminal-symbol* terminal-predicate* terminal-meta-var**)
      (let ([lang (make-language symbols)])
        (language->syntax lang))))

  (define parse-language-clause
    (lambda (who stx clause)
      (syntax-case clause (terminals)
        [(terminals terminal-clause ...)
         (values #'(terminal-clause ...) '())]
        [nonterminal-clause
         (values '() #'(nonterminal-clause))])))

  (define parse-terminal-clause
    (lambda (who stx clause)
      (syntax-case clause ()
        [(symbol (meta-var ...))
         (for-all identifier? #'(symbol meta-var ...))
         (values #'symbol #'(meta-var ...))]
        [_ (syntax-violation who "invalid terminal clause" stx clause)])))

  (define make-symbol-hashtable
    (lambda ()
      (make-hashtable symbol-hash symbol=?)))

  )
