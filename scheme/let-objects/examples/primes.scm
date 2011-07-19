#!/usr/bin/env gosh

(use gtk)
(use srfi-1)
(use srfi-2)
(use util.streams)
(use uui.let-objects)

(define (main args)
  (let ((rc  (string-append (sys-getenv "HOME") "/.gtkrc")))
    (if (file-exists? rc) (gtk-rc-add-default-file rc)))
  (gtk-init args)

  (let-objects
   (window :title "Prime Numbers"
           :width-request 200 :height-request 400
           :border-width 2 :destroy (lambda _ (gtk-main-quit))
    (vbox (menu-bar :pack-fill #f :pack-expand #f
           (menu-item :label "File"
            (menu
             (menu-item :label "Quit"
                        :activate (lambda _ (gtk-main-quit))))))
          (hseparator :pack-fill #f :pack-expand #f)
          (scrolled-window
           (hbox
            (tree-view :data-stream prime-strings
             (tree-view-column :name "Prime" :attributes '("text" 0))
             )))))

   (gtk-main))

  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sieve of Erastosthenes from SICP

; (define (stream-enumerate-interval low high)
;   (if (> low high)
;     the-empty-stream
;     (stream-cons
;      low
;      (stream-enumerate-interval (+ low 1) high))))

; (define (integers-starting-from n)
;   (stream-cons n (integers-starting-from (+ n 1))))

; (define integers (integers-starting-from 1))

; (define (divisible? x y) (= (remainder x y) 0))

; (define (sieve stream)
;   (stream-cons
;    (stream-car stream)
;    (sieve (stream-filter
;            (lambda (x)
;              (not (divisible? x (stream-car stream))))
;            (stream-cdr stream)))))

; (define primes (sieve (integers-starting-from 2)))

(stream-define (next-prime a b strm)
               (let ((c (stream-car strm)) (x (stream-cdr strm)))
                 (cond ((< b c) (next-prime a (+ a b) strm))
                       ((< c b) (stream-cons c (next-prime a b x)))
                       (else (next-prime a (+ a b) x)))))
(stream-define (sift a x) (next-prime a (+ a a) x))
(stream-define (sieve strm)
               (let ((a (stream-car strm)) (x (stream-cdr strm)))
                 (stream-cons a (sieve (sift a x)))))
(define primes (sieve (stream-from 2)))

(define prime-strings (stream-map list (stream-map number->string primes)))
