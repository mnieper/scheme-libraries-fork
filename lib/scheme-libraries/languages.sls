#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries languages)
  (export
    define-language
    +
    -
    entry
    extends
    maybe
    terminals
    language->datum)
  (import
    (rnrs)
    (scheme-libraries)
    (scheme-libraries languages $syntax-parsers))

  (define-auxiliary-syntax extends)
  (define-auxiliary-syntax entry)
  (define-auxiliary-syntax maybe)
  (define-auxiliary-syntax terminals)

  (define keyword)
  (define nonterminal)
  (define terminal)

  (define language->datum-key)
  (define extend-language-key)

  (define-syntax/who define-language
    (lambda (x)
      (define parse-clauses
        (lambda (cl*)
          (let f ([cl* cl*]
                  [extension-clause #f]
                  [entry-clause #f]
                  [terminals-clause #f]
                  [nonterminal-clauses '()])
            (if (null? cl*)
                (values extension-clause
                        entry-clause
                        terminals-clause
                        (reverse nonterminal-clauses))
                (let ([cl (car cl*)] [cl* (cdr cl*)])
                  (syntax-case cl (extends entry terminals)
                    [(extends . _)
                     (begin
                       (when extension-clause
                         (syntax-violation who "duplicate extends clause" x cl))
                       (f cl* cl entry-clause terminals-clause nonterminal-clauses))]
                    [(entry . _)
                     (begin
                       (when entry-clause
                         (syntax-violation who "duplicate entry clause" x cl))
                       (f cl* extension-clause cl terminals-clause nonterminal-clauses))]
                    [(terminals . _)
                     (begin
                       (when terminals-clause
                         (syntax-violation who "duplicate nonterminals clause" x cl))
                       (f cl* extension-clause entry-clause cl nonterminal-clauses))]
                    [(non-terminal-name . _)
                     (identifier? #'non-terminal-name)
                     (f cl* extension-clause entry-clause terminals-clause
                        (cons cl nonterminal-clauses))]
                    [_ (syntax-violation who "invalid clause" x cl)]))))))
      (define parse-extension-clause
        (lambda (cl)
          (and cl
               (syntax-case cl ()
                 [(_ language-name) (identifier? #'language-name) #'language-name]
                 [_ (syntax-violation who "invalid extends clause" x cl)]))))
      (syntax-case x ()
        [(_ language-name clause ...)
         (identifier? #'language-name)
         (let-values ([(extension-clause entry-clause terminals-clause nonterminal-clauses)
                       (parse-clauses #'(clause ...))])
           (define base-language-name (parse-extension-clause extension-clause))
           (define extended? (and base-language-name #t))
           (with-syntax ([stx x]
                         [base-language-name base-language-name]
                         [entry-clause entry-clause]
                         [terminals-clause terminals-clause]
                         [nonterminal-clauses nonterminal-clauses])
             (if extended?
                 #'(base-language-name extend-language-key stx language-name entry-clause terminals-clause nonterminal-clauses)
                 #'(define-extended-language stx language-name entry-clause terminals-clause nonterminal-clauses))))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax define-extended-language
    (lambda (x)
      (define who 'define-language)
      (define-record-type meta-variable-binding
        (nongenerative) (opaque #f))
      (define-record-type meta-variable-terminal-binding
        (nongenerative) (parent meta-variable-binding) (sealed #t)
        (fields name))
      (define-record-type meta-variable-nonterminal-binding
        (nongenerative) (parent meta-variable-binding) (sealed #t)
        (fields name))
      (define generate-language-definition
        (lambda (stx language-name entry-clause terminals nonterminals)
          (define meta-variables (make-eq-hashtable))
          (define collect-terminal-meta-variables!
            (lambda ()
              (for-each
               (lambda (terminal)
                 (syntax-case terminal ()
                   [(terminal-name (meta-var ...))
                    (for-each
                     (lambda (meta-var)
                       (hashtable-update!
                        meta-variables
                        (syntax->datum meta-var)
                        (lambda (val)
                          (when val
                            (syntax-violation who "duplicate meta-variable" stx meta-var))
                          (make-meta-variable-terminal-binding #'terminal-name))
                        #f))
                     #'(meta-var ...))]
                   [_ (assert  #f)]))
               terminals)))
          (define collect-nonterminal-meta-variables!
            (lambda ()
              (for-each
               (lambda (nonterminal)
                 (syntax-case nonterminal ()
                   [(nonterminal-name (meta-var ...) . cls)
                    (for-each
                     (lambda (meta-var)
                       (hashtable-update!
                        meta-variables
                        (syntax->datum meta-var)
                        (lambda (val)
                          (when val
                            (syntax-violation who "duplicate meta-variable" stx meta-var))
                          (make-meta-variable-nonterminal-binding #'nonterminal-name))
                        #f))
                     #'(meta-var ...))]
                   [_ (assert #f)]))
               nonterminals)))
          ;; TODO: Maybe rename production.
          (define parse-production
            (lambda (cl)
              (syntax-case cl ()
                ;; TODO: Handle <ellipsis>
                [(cl1 . cl2)
                 (with-syntax ([((var1 ...) ast1) (parse-production #'cl1)]
                               [((var2 ...) ast2) (parse-production #'cl2)])
                   #'((var1 ... var2 ...) (cons ast1 ast2)))]
                [()
                 #'(() (list))]
                [id
                 (identifier? #'id)
                 (cond
                  [(hashtable-ref meta-variables (identifier->meta-variable #'id) #f)
                   =>
                   (lambda (binding)
                     (cond
                      [(meta-variable-terminal-binding? binding)
                       (with-syntax ([meta-var (meta-variable-terminal-binding-name binding)])
                         (with-syntax ([(var) (generate-temporaries #'(meta-var))])
                           #'((var) (terminal meta-var))))]
                      [(meta-variable-nonterminal-binding? binding)
                       (with-syntax ([meta-var (meta-variable-nonterminal-binding-name binding)])
                         (with-syntax ([(var) (generate-temporaries #'(meta-var))])
                           #'((var) (nonterminal meta-var))))]
                      [else (assert #f)]))]
                  [else
                   (list (list) #'(keyword id))])]
                [_ (syntax-violation who "invalid production clause" stx cl)])))
          (define parse-productions
            (lambda (parent-name cl*)
              (let f ([cl* cl*])
                (if (null? cl*)
                    (values '() '())
                    (let-values ([(definition* production*) (f (cdr cl*))])
                      (define production (parse-production (car cl*)))
                      (with-syntax ([(record-name constructor-name predicate-name)
                                     (generate-temporaries #'(record-name constructor-name predicate-name))]
                                    [parent-name parent-name]
                                    [((var ...) . _) production])
                        (with-syntax ([(accessor ...) (generate-temporaries #'(var ...))]
                                      [production production])
                          (values
                           (cons
                            #'(define-record-type (record-name constructor-name predicate-name)
                                (nongenerative) (parent parent-name) #;(opaque #t) (sealed #t)
                                (fields (immutable var accessor) ...))
                            definition*)
                           ;; XXX: Are terminals naked or do they fulfill the predicate name?
                           (cons #'(constructor-name predicate-name (accessor ...) production)
                                 production*)))))))))
          (define parse-productions*
            (lambda ()
              (let f ([nonterminals nonterminals])
                (syntax-case nonterminals ()
                  [() (values '() '())]
                  [((nonterminal-name meta-vars cl ...) . nonterminals)
                   (with-syntax ([(record-name constructor-name) (generate-temporaries #'(record-name constructor-name))]
                                 [predicate-name (construct-name #'nonterminal-name language-name "-" #'nonterminal-name "?")])
                     (let-values ([(definition1* production1*)
                                   (f #'nonterminals)]
                                  [(definition2* production2*)
                                   (parse-productions #'record-name #'(cl ...))])
                       (values
                        (cons #'(define-record-type (record-name constructor-name predicate-name )
                                  (nongenerative) #;(opaque #t))
                              (append definition1* definition2*))
                        (append production1* production2*))))]
                  [_ (assert #f)]))))
          (when (null? nonterminals)
            (syntax-violation who "missing nonterminal clause" stx))
          (collect-terminal-meta-variables!)
          (collect-nonterminal-meta-variables!)
          (let-values ([(definition* productions) (parse-productions*)]
                       [(entry) (parse-entry-clause stx nonterminals entry-clause)])
            (with-syntax ([language-name language-name]
                          [entry entry]
                          [terminals terminals]
                          [nonterminals nonterminals]
                          [(definition ...) definition*])
              #'(begin
                  definition ...
                  (define-syntax language-name
                    (make-language-transformer #'language-name #'entry #'terminals #'nonterminals)))))))
      (syntax-case x ()
        [(_ stx language-name (base-terminal ...) (base-nonterminal ...)
            entry-clause terminals-clause (nonterminal-clause ...))
         (let ([stx #'stx])
           (define terminals (parse-extended-terminals-clause stx #'(base-terminal ...) #'terminals-clause))
           (define nonterminals (filter (lambda (cl) (not (null? cl)))
                                        (parse-extended-nonterminal-clauses stx
                                                                            #'(base-nonterminal ...)
                                                                            #'(nonterminal-clause ...))))
           (generate-language-definition stx #'language-name #'entry-clause terminals nonterminals))]
        [(_ stx language-name
            entry-clause terminals-clause (nonterminal-clause ...))
         (let ([stx #'stx])
           (define terminals (parse-terminals-clause stx #'terminals-clause))
           (define nonterminals (parse-nonterminal-clauses stx #'(nonterminal-clause ...)))
           (generate-language-definition stx #'language-name #'entry-clause terminals nonterminals))])))

  (define make-language-transformer
    (lambda (who entry terminals nonterminals)
      (lambda (x)
        (syntax-case x (extend-language-key language->datum-key)
          [(_ extend-language-key stx language-name entry-clause terminals-clause nonterminal-clauses)
           (with-syntax ([terminals terminals]
                         [nonterminals nonterminals])
             #'(define-extended-language stx language-name terminals nonterminals
                 entry-clause terminals-clause nonterminal-clauses))]
          [(_ language->datum-key)
           (with-syntax ([who-name who]
                         [entry-name entry]
                         [(terminal-clause ...) terminals]
                         [(nonterminal-clause ...) nonterminals])
             #''(define-language who-name
                  (entry entry-name)
                  (terminals terminal-clause ...)
                  nonterminal-clause ...))]
          [_ (syntax-violation who "invalid use of language name" x)]))))

  (define-syntax/who language->datum
    (lambda (x)
      (syntax-case x ()
        [(_ language-name)
         (identifier? #'language-name)
         #'(language-name language->datum-key)]
        [_ (syntax-violation who "invalid syntax" x)]))))
