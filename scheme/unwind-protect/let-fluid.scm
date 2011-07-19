(define *fluid-env* '())

(define-syntax let-fluid
  (syntax-rules ()
    ((let-fluid ((x e) ...) b ...)
     (let ((old-fluid-env *fluid-env*))
       (set! *fluid-env* 
         (append! (list (cons 'x e) ...) *fluid-env*))
       (let ((result (begin b ...)))
         (set! *fluid-env* old-fluid-env)
         result)))))

(define-syntax fluid
  (syntax-rules ()
    ((fluid x)
     (cond ((assq 'x *fluid-env*) => cdr)
           (else (error 'undefined-fluid 'x))))))

(define-syntax define-fluid
  (syntax-rules ()
    ((define-fluid x e)
     (set! *fluid-env*
       (cons (cons 'x e) *fluid-env*)))))

(define-syntax set-fluid!
  (syntax-rules ()
    ((set-fluid! x e)
     (cond ((assq 'x *fluid-env*)
            => (lambda (c)
                 (set-cdr! c e)))
           (else (error 'undefined-fluid 'x))))))

(define call/cc-f
  (let ((call/cc-orig call/cc))
    (lambda (proc)
      (call/cc-orig
        (lambda (k)
          (let* ((my-fluid-env *fluid-env*)
                 (cob (lambda (v)
                        (set! *fluid-env* my-fluid-env)
                        (k v))))
            (proc cob)))))))
