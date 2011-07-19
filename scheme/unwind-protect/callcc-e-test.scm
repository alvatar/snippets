(load "callcc-e.scm")

(define r #f)

(define test
  (lambda (bool)
    (call/cc-e
      bool
      (lambda (k)
        (unwind-protect 
          (begin
            (call/cc-e
              #f (lambda (k)
                   (set! r k)))
            (k 9))
          (printf "uw postlude~%"))))))

(printf "(test #f) followed by (r 8), no postlude, no error~%")
(printf "(test #t) followed by (r 8), postlude, error~%")

