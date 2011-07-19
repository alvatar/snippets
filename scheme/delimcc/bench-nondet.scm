; A benchmark of shift/reset: Filinski's representing non-determinism monads
; 
; Sec 6.1 of
; Martin Gasbichler, Michael Sperber: Final Shift for Call/cc: Direct
; Implementation of Shift and Reset, ICFP'02, pp. 271-282. 
; http://www-pu.informatik.uni-tuebingen.de/users/sperber/papers/shift-reset-direct.pdf

; Implementations of shift-reset
;(load "shift-reset.scm") ; Original Filinski's shift-reset
(load "delimcc-simple.scm")

;; (define p0 (new-prompt))
;; (define (reflect meaning)
;;   (shift p0 k (extend k meaning)))
;; (define (reify thunk)
;;   (push-prompt p0 (eta (thunk)))))))

; Filinski's reresentation of monads
(define (reflect meaning)
  (shift k (extend k meaning)))
(define (reify thunk)
  (reset (eta (thunk))))

; the non-determinism monad

(define (eta x) (list x))
(define (extend f l) (apply append (map f l)))

(define-syntax amb 
  (syntax-rules ()
    ((amb x ...) (amb* (lambda () x) ...))))

(define (amb* . t)
  (reflect (apply append (map reify t))))

; Two sample expressions

(define (ww)
  (let ((f (lambda (x) (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))))
    (reify (lambda () (f (amb 0 2 3 4 5 32))))))


(define (www)
  (let ((f (lambda (x) (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))))
    (reify (lambda () (f (f (amb 0 2 3 4 5 32)))))))

(define (wwww)
  (let ((f (lambda (x) (+ x (amb 6 4 2 8) (amb 2 4 5 4 1)))))
    (reify (lambda () (f (f (f (amb 0 2 3 4 5 32))))))))

(if (not (equal? (ww)
  '(8 10 11 10 7 6 8 9 8 5 4 6 7 6 3 10 12 13 12 9 10 12 13 12 9 8 10 11
10 7 6 8 9 8 5 12 14 15 14 11 11 13 14 13 10 9 11 12 11 8 7 9 10 9 6
13 15 16 15 12 12 14 15 14 11 10 12 13 12 9 8 10 11 10 7 14 16 17 16
13 13 15 16 15 12 11 13 14 13 10 9 11 12 11 8 15 17 18 17 14 40 42 43
42 39 38 40 41 40 37 36 38 39 38 35 42 44 45 44 41)))
  (error "problem"))



; Using Filinski's original implementation of shift/reset in terms
; of call/cc -- with memory leak.
; Petite Chez Scheme 6.0a

(time (length (www)))
;;     2 collections
;;     10 ms elapsed cpu time, including 0 ms collecting
;;     11 ms elapsed real time, including 0 ms collecting
;;     1819304 bytes allocated, including 2172352 bytes reclaimed
;; 2400

(time (length (wwww)))
;; (time (length (wwww)))
;;     34 collections
;;     219 ms elapsed cpu time, including 11 ms collecting
;;     225 ms elapsed real time, including 12 ms collecting
;;     37160472 bytes allocated, including 36796504 bytes reclaimed
;; 48000


;; Using delimcc-simple.scm

;; (time (length (www)))
;;     2 collections
;;     9 ms elapsed cpu time, including 0 ms collecting
;;     10 ms elapsed real time, including 1 ms collecting
;;     1521176 bytes allocated, including 2123784 bytes reclaimed
;; 2400


;; (time (length (wwww)))
;;     29 collections
;;     206 ms elapsed cpu time, including 11 ms collecting
;;     212 ms elapsed real time, including 9 ms collecting
;;     30981576 bytes allocated, including 31445128 bytes reclaimed
;; 48000
