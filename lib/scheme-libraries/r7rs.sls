#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries r7rs)
  (export
    syntax-rules)
  (import
    (only (chezscheme) trace-define-syntax)
    (rename (rnrs)
      (syntax-rules r6rs:syntax-rules))
    (scheme-libraries define-who))

  (trace-define-syntax syntax-rules
    (lambda (stx)
      (define who 'syntax-rules)
      (define ellipsis?
        (lambda (id)
          (free-identifier=? id #'(... ...))))
      (define underscore?
        (lambda (id)
          (free-identifier=? id #'_)))
      (define transform
        (lambda (ell lit* pat* tmpl*)
          (define custom-ellipsis?
            (lambda (id)
              (and ell
                   (bound-identifier=? id ell))))
          (define maybe-replace-literal*
            (lambda (ell? ell pat* guard**)
              (if (find ell? lit*)
                  (with-syntax ([(tmp) (generate-temporaries #'(tmp))]
                                [ell ell])
                    (values (map (lambda (pat)
                                   (replace #'ell #'tmp pat))
                                 pat*)
                            (map (lambda (guard*)
                                   #`((free-identifier=? #'tmp #'ell) #,@guard*))
                                 guard**)))
                  (values pat* guard**))))
          (define replace
            ;; FIXME: "old" can be more than once in pattern.
            ;; Moreover, "old" can appear at higher depths.  So, we
            ;; have to return the new pattern together with a list of
            ;; new temporaries together with their depths.
            ;; NOTE: We have to count actual ellipses to get at the depths.
            (lambda (old new pat)
              (let f ([pat pat])
                (syntax-case pat ()
                  [(x . y)
                   (cons (f #'x) (f #'y))]
                  [#(x ...)
                   (vector-map f #'#(x ...))]
                  [x
                   (identifier? #'x)
                   (if (bound-identifier=? #'x old)
                       new
                       #'x)]
                  [x #'x]))))
          (let ([guard** (map (lambda (pat) '()) pat*)])
            (let*-values ([(pat* guard**) (maybe-replace-literal* ellipsis? #'(... ...) pat* guard** )]
                          [(pat* guard**) (maybe-replace-literal* underscore? #'_ pat* guard**)]
                          [(pat* guard**) (maybe-replace-literal* custom-ellipsis? ell pat* guard**)])
              (let ([lit* (filter (lambda (lit)
                                    (not (or (ellipsis? lit)
                                             (underscore? lit)
                                             (custom-ellipsis? lit))))
                                  lit*)])
                (if ell
                    (assert #f)         ;FIXME: Implement custom ellipsis.
                    (generate lit* pat* guard** tmpl*)))))))
      (define generate
        (lambda (lit* pat* guard** tmpl*)
          (with-syntax ([(lit ...) lit*]
                        [(pat ...) pat*]
                        [((guard ...) ...) guard**]
                        [(tmpl ...) tmpl*])
            #'(lambda (stx)
                (syntax-case stx (lit ...)
                  [(_ . pat)
                   (and guard ...)
                   tmpl]
                  ...)))))
      (syntax-case stx ()
        [(_ (lit ...) [(k . pat) tmpl] ...)
         (for-all identifier? #'(k ... lit ...))
         (transform #f #'(lit ...) #'(pat ...) #'(tmpl ...))]
        [(_ ell (lit ...) [(k . pat) tmpl] ...)
         (for-all identifier? #'(ell k ... lit ...))
         (transform #'ell #'(lit ...) #'(pat ...) #'(tmpl ...))]
        [_ (syntax-violation who "invalid syntax" stx)]))))
