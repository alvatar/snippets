(define-syntax trace
  (er-macro-transformer
   (lambda (expression rename compare)
     (import matchable)
     (match-let (((_ f) expression))
       (let ((%set! (rename 'set!))
             (%lambda (rename 'lambda))
             (%call-with-values
                 (rename 'call-with-values))
             (%apply (rename 'apply))
             (%format (rename 'format))
             (%values (rename 'values))
             (%let (rename 'let))
             (%current-error-port
              (rename 'current-error-port))
             (%f (rename 'f)))
         `(,%let ((,%f ,f))
            (,%set!
             ,f
             (,%lambda x
               (,%format (,%current-error-port)
                         ";; Arguments to ~a: ~a~%"
                         ',f
                         x)
               (,%let ((return-values
                        (,%call-with-values
                            (,%lambda () (,%apply ,%f x))
                          (,%lambda x x))))
                 (,%format (,%current-error-port)
                           ";; Values from ~a: ~a~%"
                           ',f
                           return-values)
                 (,%apply ,%values return-values)))))))))))

;; and here’s what it looks like in action:
 (define (g h) (values h (+ h 1)))
 
 (trace g)
 
 (g 2)
 
;;; Output:
;; ;; Arguments to g: (2)
;; ;; Values from g: (2 3)
;; 2
;; 3
