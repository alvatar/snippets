;;;; type-errors.scm
;;;; Kon Lovett, Apr '09

;;;

(module type-errors (;export
  make-bad-argument-message
  make-error-type-message
  signal-type-error
  error-argument-type
  warning-argument-type
  (define-error-type error-argument-type)
  error-number
  error-fixnum
  error-flonum
  error-integer
  error-real
  error-complex
  error-rational
  error-exact
  error-inexact
  error-positive-number
  error-cardinal-number
  error-positive-fixnum
  error-cardinal-fixnum
  error-positive-integer
  error-cardinal-integer
  error-procedure
  error-input-port
  error-output-port
  error-list
  error-pair
  error-blob
  error-vector
  error-structure
  error-record
  error-record-type
  error-symbol
  error-keyword
  error-string
  error-char
  error-boolean
  error-alist
  error-minimum-argument-count
  error-argument-count
  error-interval
  error-closed-interval error-open-interval
  error-half-open-interval error-half-closed-interval)

  (import scheme chicken foreign (only data-structures ->string conc))

  (require-library data-structures)

  (declare
    (constant vowel?)
    (bound-to-procedure ##sys#signal-hook ##sys#error-hook) )

;;;

;;

(define (make-bad-argument-message #!optional argnam)
  (string-append "bad " (if argnam (conc #\` argnam #\' #\space) "") "argument") )

(define (vowel? ch) (and (memq ch '(#\a #\e #\i #\o #\u)) #t))

(define (make-error-type-message kndnam #!optional argnam)
  (let ((kndnam (->string kndnam)))
    (conc
      (make-bad-argument-message argnam)
      " type - not "
      (if (vowel? (string-ref kndnam 0)) "an" "a")
      #\space kndnam) ) )

;;

(define (signal-type-error loc msg . objs)
  (apply ##sys#signal-hook #:type-error loc msg objs) )

;;

(define (error-argument-type loc obj kndnam #!optional argnam)
  (signal-type-error loc (make-error-type-message kndnam argnam) obj) )

;;

(define (warning-argument-type loc obj typnam #!optional argnam)
  (warning
    (string-append 
      (if loc (conc #\( (symbol->string loc) #\) #\space) "")
      (conc (make-error-type-message typnam argnam) #\: #\space)
      (->string obj))) )

;;

; <symbol>          : <msg> is "<symbol>"
; <symbol> <string> : <msg> is <string>
; ->
; (define (error-<symbol> loc obj #!optional argnam)
;   (error-argument-type loc obj <msg> argnam) )

(define-syntax define-error-type
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (let ((_define (rnm 'define))
            (_#!optional (rnm '#!optional))
            (_error-argument-type (rnm 'error-argument-type)) )
        (let* ((typ (cadr frm))
               (typstr (symbol->string typ))
               (msg (if (null? (cddr frm)) typstr (caddr frm)))
               (nam (string->symbol (string-append "error-" typstr))) )
          `(,_define (,nam loc obj ,_#!optional argnam)
             (,_error-argument-type loc obj ,msg argnam) ) ) ) ) ) )

;;

(define-error-type number)
(define-error-type fixnum)
(define-error-type flonum)
(define-error-type integer)
(define-error-type real)
(define-error-type complex)
(define-error-type rational)
(define-error-type exact)
(define-error-type inexact)
(define-error-type positive-number)
(define-error-type cardinal-number)
(define-error-type positive-fixnum)
(define-error-type cardinal-fixnum)
(define-error-type positive-integer)
(define-error-type cardinal-integer)
(define-error-type procedure)
(define-error-type input-port)
(define-error-type output-port)
(define-error-type list)
(define-error-type pair)
(define-error-type blob)
(define-error-type vector)
(define-error-type symbol)
(define-error-type keyword)
(define-error-type string)
(define-error-type char)
(define-error-type boolean)

(define (*error-structure loc obj kndnam tag argnam)
	(error-argument-type loc obj (conc kndnam #\space tag) argnam) )

(define (error-structure loc obj tag #!optional argnam)
	(*error-structure loc obj "structure" tag argnam) )

(define (error-record loc obj tag #!optional argnam)
	(*error-structure loc obj "record" tag argnam) )

(define (error-record-type loc obj tag #!optional argnam)
	(*error-structure loc obj "record-type" tag argnam) )
 
(define-error-type alist "association-list")

(define (error-interval loc num lft min max rgt #!optional argnam)
  (##sys#signal-hook #:bounds-error loc
    (conc (make-bad-argument-message argnam) " must be in " lft min #\space max rgt)
    num) )

(define (error-closed-interval loc num min max argnam)
  (error-interval loc num '|[| min max '|]| argnam))

(define (error-open-interval loc num min max argnam)
  (error-interval loc num '|]| min max '|[| argnam))

(define (error-half-open-interval loc num min max argnam)
  (error-interval loc num '|]| min max '|]| argnam))

(define (error-half-closed-interval loc num min max argnam)
  (error-interval loc num '|[| min max '|[| argnam))

(define (error-minimum-argument-count loc argc minargc)
  (##sys#error-hook (foreign-value "C_BAD_MINIMUM_ARGUMENT_COUNT_ERROR" int) loc minargc argc) )

(define (error-argument-count loc argc maxargc)
  (##sys#error-hook (foreign-value "C_BAD_ARGUMENT_COUNT_ERROR" int) loc maxargc argc) )

) ;module type-errors
