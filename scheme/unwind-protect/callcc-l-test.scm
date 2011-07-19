(load "callcc-l.scm")

(define r #f)

(define test
  (lambda (bool)
    (call/cc-l 
      (lambda (k)
        (unwind-protect 
          (begin
            (call/cc-l 
              (lambda (k)
                   (set! r k)))
            (k bool 9))
          (printf "uw postlude~%"))))))

(printf "(test #f) followed by (r #f 8), no postlude, no error~%")
(printf "(test #t) followed by (r #f 8), postlude, error~%")


(define test2
  (lambda ()
    (call/cc-l
      (lambda (k)
        (unwind-protect
          (begin (call/cc-l
                   (lambda (k)
                     (printf "8~%")
                     (k #t 'last-use-but-no-postlude)
                     (printf "no show~%")))
                 (k #f 10))
          (printf "uw postlude~%"))))))

(printf "(test2) no postlude~%")
