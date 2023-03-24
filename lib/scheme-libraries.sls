#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries)
  (export
    construct-name
    define-auxiliary-syntax
    define/who
    define-syntax/who
    define-values
    ellipsis?
    make-parameter
    make-thread-parameter
    symbolic-identifier=?
    with-implicit

    ;; (scheme-libraries atom)
    atom?

    ;; (scheme-libraries basic-format-strings)
    format

    ;; (scheme-libraries exceptions)
    assertion-violationf
    errorf

    ;; (scheme-libraries filenames)
    filename?

    ;; (scheme-libraries format-conditions)
    &format
    make-format-condition
    format-condition?

    ;; (scheme-libraries lists)
    length+
    split-at

    ;; (scheme-libraries match)
    match
    unquote
    ...
    _
    ->
    guard

    ;; (scheme-libraries numbers)
    exact-integer?
    exact-positive-integer?
    exact-nonnegative-integer?
    nonnegative-fixnum?

    ;; (scheme-libraries ports)
    textual-input-port?
    textual-output-port?

    ;; (scheme-libraries reading annotated-datums)
    annotated-datum?
    annotated-datum-source-location
    annotated-datum-value
    make-annotated-atom
    annotated-atom?
    make-annotated-pair
    annotated-pair?
    annotated-pair-car
    annotated-pair-cdr
    make-annotated-list
    make-annotated-dotted-list
    make-annotated-vector
    annotated-vector?
    annotated-vector-ref

    ;; (scheme-libraries reading lexemes)
    lexeme?
    lexeme-start
    lexeme-end
    make-end-of-input
    end-of-input?
    make-atomic
    atomic?
    atomic-value
    make-left-parenthesis
    left-parenthesis?
    make-right-parenthesis
    right-parenthesis?
    make-left-bracket
    left-bracket?
    make-right-bracket
    right-bracket?
    make-vector-prefix
    vector-prefix?
    make-bytevector-prefix
    bytevector-prefix?
    make-abbreviation
    abbreviation?
    abbreviation-symbol
    make-dot
    dot?

    ;; (scheme-libraries reading positions)
    make-position
    position?
    position-line
    position-column
    position-lines
    position-columns
    position-tabulator

    ;; (scheme-libraries reading readers)
    make-reader
    reader?
    reader-get-annotated-datum

    ;; (scheme-libraries reading source-locations)
    make-source-location
    source-location?
    source-location-filename
    source-location-start
    source-location-end
    &source-location-condition
    make-source-location-condition
    source-location-condition?
    condition-source-location
    display-source-location

    ;; (scheme-libraries reading tokenizers)
    make-tokenizer
    tokenizer?
    tokenizer-get-lexeme
    tokenizer-lexical-error
    &lexical-error
    make-lexical-error
    lexical-error?

    ;; (scheme-libraries record-writer)
    record-writer

    ;; (scheme-libraries unicode)
    unicode-scalar-value?
    unicode-width)
  (import
    (scheme-libraries atom)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-auxiliary-syntax)
    (scheme-libraries define-who)
    (scheme-libraries define-values)
    (scheme-libraries exceptions)
    (scheme-libraries filenames)
    (scheme-libraries format-conditions)
    (scheme-libraries helpers)
    (scheme-libraries lists)
    (scheme-libraries match)
    (scheme-libraries numbers)
    (scheme-libraries ports)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries reading lexemes)
    (scheme-libraries reading positions)
    (scheme-libraries reading readers)
    (scheme-libraries reading source-locations)
    (scheme-libraries reading tokenizers)
    (scheme-libraries record-writer)
    (scheme-libraries parameters)
    (scheme-libraries thread-parameters)
    (scheme-libraries unicode)
    (scheme-libraries with-implicit)))
