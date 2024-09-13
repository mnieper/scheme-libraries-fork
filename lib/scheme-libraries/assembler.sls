#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2024).

(library (scheme-libraries assembler)
  (export
    assemble
    comment
    epilog
    func
    globl
    label)
  (import
    (rnrs)
    (scheme-libraries assembler $target)
    (scheme-libraries assembly-output)
    (scheme-libraries define-auxiliary-syntax)
    (scheme-libraries define-who))

  (define-auxiliary-syntax comment)
  (define-auxiliary-syntax epilog)
  (define-auxiliary-syntax func)
  (define-auxiliary-syntax globl)
  (define-auxiliary-syntax label)

  (define-syntax/who assemble
    (lambda (stx)
      (define parse-expression
        (lambda (expr)
          (syntax-case expr (unquote comment epilog func globl label)
            [,cmd
             #'cmd]
            [epilog
             #'(emit-epilog)]
            [(comment arg)
             (with-syntax ([quoted-arg (quote-argument #'arg)])
               #'(emit-comment quoted-arg))]
            [(label arg)
             (with-syntax ([quoted-arg (quote-argument #'arg)])
               #'(emit-label quoted-arg))]
            [(func arg)
             (with-syntax ([quoted-arg (quote-argument #'arg)])
               #'(emit-func-directive quoted-arg))]
            [(globl arg)
             (with-syntax ([quoted-arg (quote-argument #'arg)])
               #'(emit-globl-directive quoted-arg))]
            [(kwd arg ...)
             (identifier? #'kwd)
             (with-syntax ([(quoted-arg ...) (map quote-argument #'(arg ...))]
                           [emit (if (jump-instruction? (syntax->datum #'kwd)) #'emit-jump #'emit)])
               #'(emit 'kwd quoted-arg ...))]
            [lbl
             (string? (syntax->datum #'lbl))
             #'(emit-label lbl)]
            [_ (syntax-violation who "invalid expression" stx expr)])))
      (define quote-argument
        (lambda (arg)
          (syntax-case arg (unquote)
            [,unquoted-arg #'unquoted-arg]
            [arg #''arg])))
      (syntax-case stx ()
        [(_ expr1 expr2 ...)
         (with-syntax ([(cmd ...) (map parse-expression #'(expr1 expr2 ...))])
           #'(begin cmd ...))]
        [_ (syntax-violation who "invalid syntax" stx)]))))
