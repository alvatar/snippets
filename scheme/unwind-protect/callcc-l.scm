(load "let-fluid.scm")
(load "delq.scm")
(load "up-syn.scm")

(define call/cc-l #f)
(define unwind-protect-proc #f)

(define-fluid *curr-call/cc-cob* (lambda (b v) #f))
(define-fluid *curr-u-p-local-conts* '())

(define-fluid *curr-u-p-alive?* (lambda () #t))

(let ((update (list 'update))
      (delete (list 'delete)))

  (set! call/cc-l
    (lambda (proc)
      (call/cc-f
        (lambda (k)
          (set-fluid! *curr-u-p-local-conts*
            (cons k (fluid *curr-u-p-local-conts*)))
          (let* 
            ((prev-cob (fluid *curr-call/cc-cob*))
                       (my-u-p-alive?  (fluid *curr-u-p-alive?*))
                       (my-postludes '())
                       (already-used? #f)
                       (cob
                       (lambda (msg v)
                         (cond
                           ((eq? msg update)
                           (set! my-postludes (cons v my-postludes))
                           (prev-cob update v))
                           ((eq? msg delete)
                           (set! my-postludes (delq! v my-postludes))
                           (prev-cob delete v))
                           (already-used?
                           (error 'dead-continuation))
                           ((not (my-u-p-alive?))
                           (error 'dead-unwind-protect))
                           (msg 
                           (set! already-used? #t)
                           (if (not 
                                 (memq 
                                   k 
                                   (fluid *curr-u-p-local-conts*)))
                               (for-each (lambda (pl) (pl))
                                         my-postludes))
                           (k v))
                           (else (k v))))))
            (let-fluid ((*curr-call/cc-cob* cob))
              (cob #f (proc cob))))))))

  (set! unwind-protect-proc
    (lambda (body postlude)
      (let ((curr-call/cc-cob (fluid *curr-call/cc-cob*))
                              (alive? #t))
        (let-fluid ((*curr-u-p-alive?* (lambda () alive?))
                                       (*curr-u-p-local-conts* '()))
          (letrec ((pl (lambda ()
                         (set! alive? #f)
                         (postlude)
                         (curr-call/cc-cob delete pl))))
            (curr-call/cc-cob update pl)
            (let ((res (body)))
              (pl)
              res))))))
  )
