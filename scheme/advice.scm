;;;; advice.scm


(module advice (advise unadvise)
  (import scheme chicken)

  (use srfi-1 data-structures)

  (define-record advice old before after around)

  (define (mutate-procedure old proc)
    (unless (##core#check (procedure? old))
      (##sys#signal-hook #:type-error 'mutate-procedure "bad argument type - not a procedure" old))
    (let* ((n (##sys#size old))
	   (words (##core#inline "C_words" n))
	   (y (##core#inline "C_copy_block" old (make-vector words))) )
      (##sys#become! (list (cons old (proc y))))
      y) )

  (define (find-advice proc loc)
    (unless (procedure? proc) 
      (error loc "not bad argument type - not a procedure" proc) )
    (let ((len (##sys#size proc)))
      (let loop ((i 1))
	(cond ((fx>= i len) #f)
	      ((advice? (##sys#slot proc i))
	       (##sys#slot proc i) )
	      (else (loop (fx+ i 1)))))))

  (define (((make-advisor a) o) . args)
    (for-each (lambda (ba) ((cdr ba) args)) (advice-before a))
    (let ((ao (advice-around a))
	  (aa (advice-after a)) )
      (if (null? ao)
	  (if (null? aa)
	      (apply o args)
	      (let ((results (receive (apply o args))))
		(for-each (lambda (aa) ((cdr aa) results)) aa)
		(apply values results) ) )
	  ;; this should actually be constructed on changing the around list and not
	  ;; at runtime ...
	  (let ((run 
		 (let loop ((ao (advice-around a)))
		   (let ((r (cdr ao)))
		     (if (null? r)
			 (lambda args ((cdar ao) o args))
			 (let ((next (loop (cdr ao))))
			   (lambda args ((cdar ao) next args)) ) ) ) ) ) )
	    (if (null? aa)
		(apply run args)
		(let ((results (receive (apply run args))))
		  (for-each (lambda (aa) ((cdr aa) results)) aa) 
		  (apply values results) ) ) ) ) ) )

  (define (advise mode proc h #!optional (id (gensym)))
    ;; chicken's closure representation ensures that the advice object
    ;; is part of the advisor closure, but we must avoid boxing,
    ;; so no assignment to a2 is allowed.
    (let* ((a (find-advice proc 'advise))
	   (a2 (or a (make-advice #f '() '() '()) ) ) )
      (unless a
	(advice-old-set! a2 (mutate-procedure proc (make-advisor a2))))
      (case mode
	((before) 
	 (advice-before-set! a2 (alist-cons id h (advice-before a2))) )
	((after)
	 (advice-after-set! a2 (append (advice-after a2) (list (cons id h)))))
	((around)
	 (advice-around-set! a2 (alist-cons id h (advice-around a2))))
	(else (error 'advise "invalid advice mode" mode)))
      id) )

  (define (unadvise proc #!optional id mode)
    (let ((a (or (find-advice proc 'unadvise)
		 (error 'unadvise "procedure is not advised" proc))))
      (cond (id
	     (let* ((ba (advice-before a))
		    (ae (and (or (not mode) (eq? mode 'before))
			     (assq id ba)) ) )
	       (if ae 
		   (advice-before-set! a (delete! ae ba))
		   (let* ((aa (advice-after a))
			  (ae (and (or (not mode) (eq? mode 'after))
				   (assq id aa)) ) )
		     (if ae
			 (advice-after-set! a (delete! ae aa))
			 (let* ((ao (advice-around a))
				(ae (and (or (not mode) (eq? mode 'around))
					 (assq id ao)) ) )
			   (if ae
			       (advice-around-set! a (delete ae ao)) 
			       (error 'unadvise "no such advice" id proc mode) ) ) ) ) ) ) )
	    ((memq mode '(#f around before after))
	     (when (or (not mode) (eq? mode 'before))
	       (advice-before-set! a '()))
	     (when (or (not mode) (eq? mode 'after))
	       (advice-after-set! a '()))
	     (when (or (not mode) (eq? mode 'around))
	       (advice-around-set! a '())) )
	    (else (error 'unadvise "invalid advice mode" mode)) )
      (when (and (null? (advice-before a))
		 (null? (advice-after a))
		 (null? (advice-around a)) )
	(mutate-procedure proc (constantly (advice-old a))) )
      (void) ) )

  )
