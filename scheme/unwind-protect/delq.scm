(define delq!
  (lambda (x s)
    (let loop ((s s))
      (cond ((null? s) s)
            ((eq? (car s) x) (loop (cdr s)))
            (else (set-cdr! s (loop (cdr s)))
                  s)))))
