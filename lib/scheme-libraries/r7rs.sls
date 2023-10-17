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
          (define actual-ellipsis?
            (lambda (id)
              ;; TODO: Speed up test.
              (and (identifier? id)
                   (if ell
                       (and (not (find ell lit*))
                            (custom-ellipsis? id))
                       (and (not (find ellipsis? lit*))
                            (ellipsis? id))))))
          (define maybe-replace-literal*
            (lambda (lit? lit pat* guard**)
              (define replace-literal
                (lambda (pat guard*)
                  (let-values ([(pat guards)
                                (replace-in-pattern lit pat)])
                    (values pat
                            #`(#,@guard* #,@guards)))))
              (if (find lit? lit*)
                  (let f ([pat* pat*] [guard** guard**])
                    (if (null? pat*)
                        (values '() '())
                        (let-values ([(pat guard*)
                                      (replace-literal (car pat*) (car guard**))]
                                     [(pat* guard**)
                                      (f (cdr pat*) (cdr guard**))])
                          (values (cons pat pat*)
                                  (cons guard* guard**)))))
                  (values pat* guard**))))
          (define replace-in-pattern
            (lambda (id pat)
              (let f ([pat pat] [depth #'()])
                (syntax-case pat ()
                  [(x . y)
                   (let g ([y #'y] [depthx depth])
                     (syntax-case y ()
                       [(::: . y)
                        (actual-ellipsis? #':::)
                        (g #'y #`((... ...) #,@depthx))]
                       [y
                        (let-values ([(x guardsx) (f #'x depthx)]
                                     [(y guardsy) (f #'y depth)])
                          (values #`(#,x #;ELLIPSES!!! . #,y
                                                       )
                                  (append guardsx guardsy)))]))]
                  [#(x ...)
                   (let-values ([(x* guards) (f #'(x ...) depth)])
                     (values #`#(#,@x*) guards))]
                  [x
                   (identifier? #'x)
                   ;; FIXME: bound-identifier=? does not yet work
                   ;; here.  #'x is from the user; id from us.  We
                   ;; must use id from them! (=> It appears in the
                   ;; literals.  For each free-id=? to _ or ell, we
                   ;; must do this translation!
                   (if (bound-identifier=? #'x id)
                       (and (assert #f)
                            (with-syntax ([(tmp) (generate-temporaries #'(tmp))])
                              (values #'tmp
                                      ;; XXX: Do we need quote-syntax here for ID here?
                                      ;; The user could have bound ID to a pattern variable.
                                      #`(for-all (lambda (e) (free-identifier=? e #'(... #,id)))
                                                 #'(tmp #,@depth)))))
                       (values #'x '()))]
                  [x (values #'x '())]))))
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
