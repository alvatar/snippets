(define-syntax unwind-protect
  (syntax-rules ()
    ((unwind-protect body postlude)
     (unwind-protect-proc 
       (lambda () body) (lambda () postlude)))))
