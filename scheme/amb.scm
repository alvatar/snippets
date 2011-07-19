;;; The universal backtracking ambivalence operator.
;;;
;;; Copyright (c) 2009 by Thomas Chust <chust@web.de>
;;;
;;; This package is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; Lesser General Public License for more details.
;;;
;;; References:
;;;   Dorai Sitaram, Teach Yourself Scheme in Fixnum Days.
;;;   <http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme.html>
;;;
;;;   The Free Software Foundation, The GNU Lesser General Public License.
;;;   <http://www.gnu.org/licenses/lgpl-3.0.html>
(package* amb/v1.0.0
  (provide:
    (define (amb-fail))
    (define (amb-call . thunks))

    (define-macro (amb . exprs)
      (if (null? exprs)
	'(amb-fail)
	`(amb-call
	   ,@(map
	       (lambda (expr)
		 `(lambda () ,expr))
	       exprs))))

    (define-macro (amb-assert cond?)
      `(or ,cond? (amb-fail)))

    (define* (call-with-amb-prompt thunk (failure _)))

    (define-macro (amb-find . body)
      `(call-with-amb-prompt (lambda () ,@body)))

    (define-macro (amb-collect . body)
      (let (($result (gensym 'result)))
	`(let ((,$result '()))
	   (call-with-amb-prompt
	     (lambda ()
	       (set! ,$result
		 (cons (begin ,@body) ,$result))
	       (amb-fail))
	     (lambda ()
	       (reverse ,$result)))))))

  (author: "Thomas Chust <chust@web.de>")
  (description: "The universal backtracking ambivalence operator.")
  (keywords: logic)
  (license: lgpl/v3.0)
  
  (cond-expand
    (gambit
      )
    (else
      (require: gensym/v1.0))))

;; Internal list of restart points
(cond-expand
  ((or chez chicken gambit gauche mzscheme stklos)
    (define amb-restarts
      (make-parameter '())))
  (else
    (define amb-restarts
      (let ((restarts '()))
	(lambda args
	  (if (null? args)
	    restarts
	    (set! restarts (car args))))))))

;; Signal a failure of the current evaluation branch.
;; Resumes at a restart point if possible and throws
;; an error otherwise.
(define (amb-fail)
  (let ((restarts (amb-restarts)))
    (cond
      ((null? restarts)
	(error "amb: no ambivalence context"))
      (else
	(let ((hole (caar restarts))
	      (thunk (cdar restarts))
	      (more-restarts (cdr restarts)))
	  (amb-restarts more-restarts)
	  (hole (thunk)))))))

;; Adds its return continuation paired with thunks producing possible
;; values to throw to it into the list of restart points. Invokes the
;; first restart right away.
(define (amb-call . thunks)
  (let ((restarts (amb-restarts)))
    (cond
      ((null? restarts)
       (error "amb: no ambivalence context"))
      (else
	(call-with-current-continuation
	  (lambda (hole)
	    (amb-restarts
	      (let more-restarts ((thunks thunks))
		(if (null? thunks)
		  restarts
		  (let ((thunk (car thunks))
			(more-thunks (cdr thunks)))
		    (cons
		      (cons hole thunk)
		      (more-restarts more-thunks))))))
	    (amb-fail)))))))

;; Sets up an ambivalence context to determine successful walks
;; through the expression tree. If the main thunk fails, the failure
;; thunk is invoked to produce a result; it defaults to producing an
;; error indicating that all ambivalent expressions have been
;; exhausted.
(cond-expand
  ((or chez chicken gambit gauche mzscheme stklos)
    (define* (call-with-amb-prompt
	       thunk
	       (failure (lambda ()
		 (error "amb: alternatives exhausted"))))
      (call-with-current-continuation
	(lambda (return)
	  (parameterize ((amb-restarts (list (cons return failure))))
	    (thunk))))))
  (else
    (define* (call-with-amb-prompt
	       thunk
	       (failure (lambda ()
		 (error "amb: alternatives exhausted"))))
      (call-with-current-continuation
	(lambda (return)
	  (let ((old-restarts '()))
	    (dynamic-wind
	      (lambda ()
		(set! old-restarts (amb-restarts))
		(amb-restarts (list (cons return failure))))
	      thunk
	      (lambda ()
		(amb-restarts old-restarts)))))))))

;; Simple tests
(test*
  (expect*
    (equal? (call-with-amb-prompt
	      (lambda ()
	        (amb))
	      (lambda ()
		#f))
	    #f))
  (expect*
    (equal? (amb-find (amb 1 2 3))
	    1))

  (expect*
    (equal? (amb-collect (amb))
	    '()))
  (expect*
    (equal? (amb-collect (amb 1 2 3))
	    '(1 2 3))))

;; A complex example as a test
(test*
  (define (xor a b)
    (and (or a b) (not (and a b))))

  (define (solve-kalotan-puzzle)
    (let ((parent1 (amb 'm 'f))
          (parent2 (amb 'm 'f))
          (kibi (amb 'm 'f))
          (kibi-self-desc (amb 'm 'f))
          (kibi-lied? (amb #t #f)))
      (amb-assert
       (not (eqv? parent1 parent2)))
      (amb-assert
       (if (eqv? kibi 'm)
           (not kibi-lied?)))
      (amb-assert
       (if kibi-lied?
           (xor
            (and (eqv? kibi-self-desc 'm)
                 (eqv? kibi 'f))
            (and (eqv? kibi-self-desc 'f)
                 (eqv? kibi 'm)))))
      (amb-assert
       (if (not kibi-lied?)
           (xor
            (and (eqv? kibi-self-desc 'm)
                 (eqv? kibi 'm))
            (and (eqv? kibi-self-desc 'f)
                 (eqv? kibi 'f)))))
      (amb-assert
       (if (eqv? parent1 'm)
           (and
            (eqv? kibi-self-desc 'm)
            (xor
             (and (eqv? kibi 'f)
                  (eqv? kibi-lied? #f))
             (and (eqv? kibi 'm)
                  (eqv? kibi-lied? #t))))))
      (amb-assert
       (if (eqv? parent1 'f)
           (and
            (eqv? kibi 'f)
            (eqv? kibi-lied? #t))))
      (list parent1 parent2 kibi)))
  
  (expect*
    (equal? (call-with-amb-prompt solve-kalotan-puzzle)
	    '(f m f))))
