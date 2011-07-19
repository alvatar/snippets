;;;; srfi-45.scm
;;;; Kon Lovett, May '09

;; Issues
;;
;; - All operations inlined & primitive due to high-performance nature.
;;
;; - This has been heavily modified from the original in order to extend
;; rather than supplant the R5RS 'delay'.

;;; Prelude

(declare
  (usual-integrations)
  (disable-interrupts)
  (fixnum)
  (local)
  (inline)
  (no-procedure-checks)
  (bound-to-procedure
    ##sys#signal-hook))

(include "chicken-primitive-object-inlines")

;;; Module srfi-45

(module srfi-45 (;export
  ; SRFI 45
  (lazy make-lazy-promise)
  (eager make-eager-promise)
  delay
  promise?
  force
  ; Extras
  lazy-promise?
  eager-promise?
  recursive-promise?)

(import (rename scheme (force r5rs:force) (delay r5rs:delay))
        (rename chicken (promise? r5rs:promise?))
        type-errors)

;; Recursive promise

(define-inline (%make-promise-box tag val) (cons tag val))
(define-inline (%promise-box-tag prmbox) (%car prmbox))
(define-inline (%promise-box-tag-set! prmbox tag) (%set-car!/mutate prmbox tag))
(define-inline (%promise-box-value prmbox) (%cdr prmbox))
(define-inline (%promise-box-value-set! prmbox val) (%set-cdr! prmbox val))

(define-inline (%eager-promise-box? prmbox) (%eq? 'eager (%promise-box-tag prmbox)))
(define-inline (%lazy-promise-box? prmbox) (%eq? 'lazy (%promise-box-tag prmbox)))

(define-inline (%make-promise tag val) (%make-structure 'recursive-promise (%make-promise-box tag val)))
(define-inline (%promise? obj) (%structure-instance? obj 'recursive-promise))
(define-inline (%promise-box prm) (%structure-ref prm 1))
(define-inline (%promise-box-set! prm prmbox) (%structure-set! prm 1 prmbox))

(define-inline (%make-eager-promise val) (%make-promise 'eager val))
(define-inline (%make-lazy-promise val) (%make-promise 'lazy val))

(define-inline (%eager-promise? obj) (and (%promise? obj) (%eager-promise-box? (%promise-box obj))))
(define-inline (%lazy-promise? obj) (and (%promise? obj) (%lazy-promise-box? (%promise-box obj))))

;; Errors

(define-error-type promise)
(define-error-type promise-valid "valid promise")
(define-error-type promise-unforced-lazy "unforced lazy promise")

;; Constructors

(define (make-lazy-promise thunk) (%make-lazy-promise thunk))
(define (make-eager-promise ls) (%make-eager-promise ls))

(define-syntax lazy (syntax-rules () ((_ ?expr) (make-lazy-promise (lambda () ?expr)))))
(define-syntax eager (syntax-rules () ((_ ?expr) (make-eager-promise (receive ?expr)))))
(define-syntax delay (syntax-rules () ((_ ?expr) (lazy (eager ?expr)))))

;; Predicates

(define (lazy-promise? obj) (%lazy-promise? obj))
(define (eager-promise? obj) (%eager-promise? obj))
(define (recursive-promise? obj) (%promise? obj))

(define (promise? obj) (or (r5rs:promise? obj) (%promise? obj)))

;; Force

(define (force prm)
  ; What kind of promise?
  (cond
    ; New fashion promise?
    ((%promise? prm)
      ; Unbox
      (let* ((prmbox (%promise-box prm))
             (value (%promise-box-value prmbox)))
        ; Process by kind
        (case (%promise-box-tag prmbox)
          ; Eager has value ready
          ((eager)
            (apply values value) )
          ; Force a lazy promise's value
          ((lazy)
            ; Better be an un-evaluated thunk
            (if (%procedure? value)
                ; Force the promise by invoking the thunk
                (let ((value* (receive (value))))
                  ; Re-fetch and check the top promise again in case it recursed into `force'
                  (let ((prmbox (and (%promise? prm) (%promise-box prm))))
                    ; Forced value, R5RS or Eager promise?
                    (if (or (not prmbox) (%eager-promise-box? prmbox)) (force prm)
                        ; Value better be a promise (and only a promise)
                        (let ((prm* (and (= 1 (length value*)) (%car value*))))
                          (cond
                            ((%promise? prm*)
                              ; Copy the promise to the top
                              (let ((prmbox* (%promise-box prm*)))
                                (%promise-box-tag-set! prmbox (%promise-box-tag prmbox*))
                                (%promise-box-value-set! prmbox (%promise-box-value prmbox*)) )
                              (%promise-box-set! prm* prmbox)
                              (force prm) )
                            ((r5rs:promise? prm*)
                              (r5rs:force prm*) )
                            (else
                              (error-promise 'force value*) ) ) ) ) ) )
                ; This shouldn't happen
                (error-promise-unforced-lazy 'force value) ) )
          ; This shouldn't happen
          (else
            (error-promise-valid 'force prm) ) ) ) )
    ; Old fashion promise?
    ((r5rs:promise? prm)
      (r5rs:force prm) )
    ; Not a promise at all. Return object per the Chicken manual.
    (else
      prm ) ) )

;;;

(register-feature! 'srfi-45)

) ;module srfi-45

#|
Copyright (C) André van Tonder (2003). All Rights Reserved.


Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:


The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.


THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#
