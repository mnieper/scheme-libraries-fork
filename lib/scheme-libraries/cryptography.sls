#!r6rs

(library (scheme-libraries cryptography)
  (export
    sha-1)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who sha-1
    (lambda (msg)
      (unless (bytevector? msg)
        (assertion-violation who ""))


      ))

  )
