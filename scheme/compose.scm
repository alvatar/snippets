;;;;;; Functional composition operators                  -*- Scheme -*-

;;; Taylor Campbell wrote this code; he places it in the public domain.

(define (recursive-compose . fs)
  (let recur ((fs fs))
    (if (null? fs)
        values
        (let ((f (car fs))
              (g (recur (cdr fs))))
          (lambda args
            (call-with-values
                (lambda () (apply g args))
              f)))))

(define (tail-recursive-compose . fs)
  (let loop ((g values) (fs fs))
    (if (null? fs)
        g
        (loop (lambda args
                (call-with-values
                  (lambda () (apply (car fs) args))
                  g))
              (cdr fs)))))
