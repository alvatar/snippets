;;;; type-checks.scm
;;;; Kon Lovett, Apr '09

; Chicken Generic Arithmetic!

;;;

(module type-checks (;export
  define-check-type
  define-check+error-type
  check-number
  check-fixnum
  check-flonum
  check-integer
  check-real
  check-complex
  check-rational
  check-exact
  check-inexact
  check-positive-fixnum
  check-cardinal-fixnum
  check-positive-integer
  check-cardinal-integer
  check-positive-number
  check-cardinal-number
  check-procedure
  check-input-port
  check-output-port
  check-list
  check-pair
  check-blob
  check-vector
  check-structure
  check-record
  check-record-type
  check-symbol
  check-keyword
  check-string
  check-char
  check-boolean
  check-alist
  check-minimum-argument-count check-argument-count
  check-closed-interval check-open-interval
  check-half-closed-interval check-half-open-interval)

  (import chicken scheme type-errors)

  (require-library type-errors)

  (declare (bound-to-procedure ##sys#structure?))

;;

(cond-expand

  (unsafe

    (define-syntax define-check-type
      (er-macro-transformer
        (lambda (frm rnm cmp)
          (let ((_define (rnm 'define)))
            (let* ((typ (cadr frm))
                   (nam (string->symbol (string-append "check-" (symbol->string typ)))) )
              `(,_define (,nam . _) (begin) ) ) ) ) ) )

    (define (check-positive-fixnum . _) (begin))
    (define (check-cardinal-fixnum . _) (begin))
    (define (check-positive-integer . _) (begin))
    (define (check-cardinal-integer . _) (begin))
    (define (check-positive-number . _) (begin))
    (define (check-cardinal-number . _) (begin))
    (define (check-structure . _) (begin))
    (define (check-record . _) (begin))
    (define (check-record-type . _) (begin))
    (define (check-minimum-argument-count . _) (begin))
    (define (check-argument-count . _) (begin))
    (define (check-closed-interval . _) (begin))
    (define (check-open-interval . _) (begin))
    (define (check-half-closed-interval . _) (begin))
    (define (check-half-open-interval . _) (begin)) )

  (else

    (export alist?)

    (define (alist? obj)
      (or (null? obj)
          (and (list? obj)
               (let loop ((ls obj))
                 (or (null? ls)
                     (and (pair? (car ls))
                          (loop (cdr ls) ) ) ) ) ) ) )

    ;;

    ; <symbol>          : <pred> is '<symbol>?'
    ; <symbol> <symbol> : <pred> is <symbol>
    ; ->
    ; (define (check-<symbol> loc obj #!optional argnam)
    ;   (unless (<pred> obj)
    ;     (error-<symbol> loc obj argnam) ) )

    (define-syntax define-check-type
      (er-macro-transformer
        (lambda (frm rnm cmp)
          (let ((_define (rnm 'define))
                (_#!optional (rnm '#!optional)) )
            (let* ((typ (cadr frm))
                   (typstr (symbol->string typ))
                   (pred (if (not (null? (cddr frm))) (caddr frm) (string->symbol (string-append typstr "?"))))
                   (nam (string->symbol (string-append "check-" typstr)))
                   (errnam (string->symbol (string-append "error-" typstr))) )
              `(,_define (,nam loc obj ,_#!optional argnam)
                 (unless (,pred obj)
                   (,errnam loc obj argnam) ) ) ) ) ) ) )

    ;;

    (define (check-positive-fixnum loc obj #!optional argnam)
      (unless (and (fixnum? obj) (fx< 0 obj))
        (error-positive-fixnum loc obj argnam) ) )

    (define (check-cardinal-fixnum loc obj #!optional argnam)
      (unless (and (fixnum? obj) (fx<= 0 obj))
        (error-cardinal-fixnum loc obj argnam) ) )

    ;;

    (define (check-positive-integer loc obj #!optional argnam)
      (unless (and (integer? obj) (positive? obj))
        (error-positive-integer loc obj argnam) ) )

    (define (check-cardinal-integer loc obj #!optional argnam)
      (unless (and (integer? obj) (<= 0 obj))
        (error-cardinal-integer loc obj argnam) ) )

    ;;

    (define (check-positive-number loc obj #!optional argnam)
      (unless (positive? obj)
        (error-positive-number loc obj argnam) ) )

    (define (check-cardinal-number loc obj #!optional argnam)
      (unless (<= 0 obj)
        (error-cardinal-number loc obj argnam) ) )

    ;;

    (define (check-structure loc obj tag #!optional argnam)
      (unless (##sys#structure? obj tag)
        (error-structure loc obj tag argnam) ) )

    (define (check-record loc obj tag #!optional argnam)
      (unless (##sys#structure? obj tag)
        (error-record loc obj tag argnam) ) )

    (define (check-record-type loc obj tag #!optional argnam)
      (unless (##sys#structure? obj tag)
        (error-record-type loc obj tag argnam) ) ) ) )

;;

(define-check-type fixnum)
(define-check-type flonum)
(define-check-type integer)
(define-check-type real)
(define-check-type complex)
(define-check-type rational)
(define-check-type exact)
(define-check-type inexact)
(define-check-type number)
(define-check-type symbol)
(define-check-type keyword)
(define-check-type string)
(define-check-type char)
(define-check-type boolean)
(define-check-type procedure)
(define-check-type input-port)
(define-check-type output-port)
(define-check-type list)
(define-check-type pair)
(define-check-type blob)
(define-check-type vector)
(define-check-type alist)

; closed interval
(define (check-closed-interval loc num min max #!optional argnam)
  (unless (and (<= min num) (<= num max))
    (error-closed-interval loc num min max argnam) ) )

; open interval
(define (check-open-interval loc num min max #!optional argnam)
  (unless (and (< min num) (< num max))
    (error-open-interval loc num min max argnam) ) )

; closed+open interval
(define (check-half-open-interval loc num min max #!optional argnam)
  (unless (and (< min num) (<= num max))
    (error-half-open-interval loc num min max argnam) ) )

; open+closed interval
(define (check-half-closed-interval loc num min max #!optional argnam)
  (unless (and (<= min num) (< num max))
    (error-half-closed-interval loc num min max argnam) ) )

(define (check-minimum-argument-count loc argc minargc)
  (unless (fx<= minargc argc)
    (error-minimum-argument-count loc argc minargc)) )

(define (check-argument-count loc argc maxargc)
  (unless (fx<= argc maxargc)
    (error-argument-count loc argc maxargc)) )

;;

; <type-symbol> [<type-predicate> [<message-string>]]

(define-syntax define-check+error-type
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (let ((_define-check-type (rnm 'define-check-type))
            (_define-error-type (rnm 'define-error-type)) )
        (let* ((typ (cadr frm))
               (pred (and (not (null? (cddr frm))) (caddr frm)))
               (mesg (and pred (not (null? (cdddr frm))) (cadddr frm))) )
          `(begin
             (,_define-error-type ,typ ,@(if mesg `(,mesg) '()))
             (,_define-check-type ,typ ,@(if pred `(,pred) '())) ) ) ) ) ) )

) ;module type-checks
