(load "let-fluid.scm")
(load "delq.scm")
(load "up-syn.scm")

(define call/cc-e #f)
(define unwind-protect-proc #f)

(define-fluid *curr-call/cc-cob* 
  (lambda (v) (lambda (x) #f)))
(define-fluid *curr-u-p-alive?* (lambda () #t))

(let ((update (list 'update))
      (delete (list 'delete)))

  (set! call/cc-e
    (lambda (once? proc)
      (if once?
          (call/cc-f
            (lambda (k)
              (let* 
                ((cob (fluid *curr-call/cc-cob*))
                 (my-postludes '())
                 (already-used? #f)
                 (cob
                   (lambda (v)
                     (cond 
                       ((eq? v update)
                        (lambda (pl)
                          (set! my-postludes
                            (cons pl my-postludes))
                          ((cob update) pl)))
                       ((eq? v delete)
                        (lambda (pl)
                          (set! my-postludes
                            (delq!  pl my-postludes))
                          ((cob delete) pl)))
                       (already-used?
                         (error 'dead-continuation))
                       (else
                         (set! already-used?  #t)
                         (for-each 
                           (lambda (pl) (pl))
                           my-postludes)
                         (k v))))))
               (let-fluid ((*curr-call/cc-cob* cob))
                 (cob (proc cob))))))
           (call/cc-f
             (lambda (k)
               (let* 
                 ((my-u-p-alive? (fluid *curr-u-p-alive?*))
                  (cob
                    (lambda (v)
                      (if (my-u-p-alive?)
                          (k v)
                          (error 'dead-unwind-protect)))))
                 (cob (proc cob))))))))

  (set! unwind-protect-proc
    (lambda (body postlude)
      (let ((curr-call/cc-cob (fluid *curr-call/cc-cob*))
            (alive? #t))
        (let-fluid ((*curr-u-p-alive?* (lambda () alive?)))
          (letrec ((pl (lambda ()
                         (set! alive? #f)
                         (postlude)
                         ((curr-call/cc-cob delete) pl))))
            ((curr-call/cc-cob update) pl)
            (let ((res (body)))
              (pl)
              res))))))

  )
