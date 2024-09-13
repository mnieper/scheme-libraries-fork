#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2024).

(library (scheme-libraries assembler $target)
  (export
    jump-instruction?)
  (import
    (rnrs)
    (scheme-libraries assembly-output $common)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries gensyms)
    (scheme-libraries match))

  (define jump-instruction?
    (lambda (ins)
      (memq ins '(jmp call jz jnz jl jle je jge jg jb jbe jae ja)))))
