;;;; inline-type-checks.scm
;;;; Kon Lovett, Apr '09

;; Needs "chicken-primitive-object-inlines.scm"
;; This source is to be included

;;

(cond-expand

  (unsafe
 
    (define-syntax define-inline-check-type
      (er-macro-transformer
        (lambda (form r c)
          (let ((_define-inline (r 'define-inline)))
            (let* ((typ (cadr form))
                   (nam (string->symbol (string-append "check-" (symbol->string typ)))) )
              `(,_define-inline (,nam . _) (begin) ) ) ) ) ) )

    (define-inline (%check-positive-fixnum . _) (begin))
    (define-inline (%check-cardinal-fixnum . _) (begin))
    (define-inline (%check-positive-integer . _) (begin))
    (define-inline (%check-cardinal-integer . _) (begin))
    (define-inline (%check-positive-number . _) (begin))
    (define-inline (%check-cardinal-number . _) (begin))
    (define-inline (%check-structure . _) (begin))
    (define-inline (%check-minimum-argument-count . _) (begin))
    (define-inline (%check-argument-count . _) (begin)) )

  (else

    (define-inline (%alist? obj)
      (or (%null? obj)
          (and (%pair? obj) (%list-every/1 (lambda (x) (%pair? x)) obj))) )

    ;;

    (define-syntax define-inline-check-type
      (er-macro-transformer
        (lambda (form r c)
          (let ((_define-inline (r 'define-inline))
                (_#!optional (r '#!optional)) )
            (let* ((typ (cadr form))
                   (typstr (symbol->string typ))
                   (pred (if (not (null? (cddr form))) (caddr form)
                             (string->symbol (string-append "%" typstr "?"))))
                   (nam (string->symbol (string-append "%check-" typstr)))
                   (errnam (string->symbol (string-append "error-" typstr))) )
              `(,_define-inline (,nam loc obj ,_#!optional argnam)
                 (unless (,pred obj)
                   (,errnam loc obj argnam) ) ) ) ) ) ) )

    ;;

    (define-inline (%check-positive-fixnum loc obj #!optional argnam)
      (unless (and (%fixnum? obj) (%fxpositive? obj))
        (error-positive-fixnum loc obj argnam) ) )

    (define-inline (%check-cardinal-fixnum loc obj #!optional argnam)
      (unless (and (%fixnum? obj) (%fxcardinal? obj))
        (error-cardinal-fixnum loc obj argnam) ) )

    ;;

    (define-inline (%check-positive-integer loc obj #!optional argnam)
      (unless (and (%integer? obj) (%positive? obj))
        (error-positive-integer loc obj argnam) ) )

    (define-inline (%check-cardinal-integer loc obj #!optional argnam)
      (unless (and (%integer? obj) (%cardinal? obj))
        (error-cardinal-integer loc obj argnam) ) )

    ;;

    (define-inline (%check-positive-number loc obj #!optional argnam)
      (unless (%positive? obj)
        (error-positive-number loc obj argnam) ) )

    (define-inline (%check-cardinal-number loc obj #!optional argnam)
      (unless (%cardinal? obj)
        (error-cardinal-number loc obj argnam) ) )

    ;;

    (define-inline (%check-structure loc obj tag #!optional argnam)
      (unless (%structure-instance? obj tag)
        (error-structure loc obj tag argnam) ) )

    ;;

    (define-inline (%check-minimum-argument-count loc argc minargc)
      (unless (%fx<= minargc argc)
        (error-minimum-argument-count loc argc minargc)) )

    (define-inline (%check-argument-count loc argc maxargc)
      (unless (%fx<= argc maxargc)
        (error-argument-count loc argc maxargc)) ) ) )

;;

(define-inline-check-type fixnum)
(define-inline-check-type flonum)
(define-inline-check-type integer)
(define-inline-check-type number)
(define-inline-check-type symbol)
(define-inline-check-type keyword)
(define-inline-check-type string)
(define-inline-check-type char)
(define-inline-check-type boolean)
(define-inline-check-type procedure)
(define-inline-check-type input-port)
(define-inline-check-type output-port)
(define-inline-check-type list)
(define-inline-check-type pair)
(define-inline-check-type blob)
(define-inline-check-type vector)
(define-inline-check-type alist)
