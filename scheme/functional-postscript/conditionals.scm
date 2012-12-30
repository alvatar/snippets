;;; handy syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define-syntax when
  (syntax-rules ()
    ((when bool body1 body2 ...)
     (if bool (begin body1 body2 ...)))))


#;(define-syntax unless
  (syntax-rules ()
    ((unless bool body1 body2 ...)
     (if (not bool) (begin body1 body2 ...)))))

(define-syntax ?	; ? is synonym for COND.
  (syntax-rules ()
    ((? clause ...) (cond clause ...))))


;;; Like CASE, but you specify the key-comparison procedure.
;;; SWITCH evaluates its keys each time through the conditional.
;;; SWITCHQ keys are not evaluated -- are simply constants.
;;; (switchq string=? (vector-ref vec i)
;;;   (("plus" "minus") ...)
;;;   (("times" "div")  ...)
;;;   (else ...))

(define-syntax switchq
  (syntax-rules ()
    ((switchq compare key clause ...)
     (let ((k key)			; Eval KEY and COMPARE
	   (c compare))			; just once, then call %switch.
       (%switchq c k clause ...)))))	; C, K are vars, hence replicable.

(define-syntax %switchq
  (syntax-rules (else)
    ((%switchq compare key ((key1 ...) body1 . body2) . rest)
     (if (or (compare key 'key1) ...)
	 (begin body1 . body2)
	 (%switchq compare key . rest)))

    ((%switchq compare key ((key1 ...)) . rest)	; Null body.
     (if (not (or (compare key 'key1) ...))
	 (%switchq compare key . rest)))
    
    ((%switchq compare key (else . body))
     (begin . body))

    ((%switchq compare key) '#f)))

(define-syntax switch
  (syntax-rules ()
    ((switch compare key clause ...)
     (let ((k key)			; Eval KEY and COMPARE
	   (c compare))			; just once, then call %switch.
       (%switch c k clause ...)))))	; C, K are vars, hence replicable.

(define-syntax %switch
  (syntax-rules (else)
    ((%switch compare key ((key1 ...) body1 body2 ...) rest ...)
     (if (or (compare key key1) ...)
	 (begin body1 body2 ...)
	 (%switch compare key rest ...)))

    ((%switch compare key ((key1 ...)) rest ...)	; Null body.
     (if (not (or (compare key key1) ...))
	 (%switch compare key rest ...)))
    
    ((%switch compare key (else body ...))
     (begin body ...))

    ((%switch compare key) '#f)))

;;; I can't get this to work -- S48 complains "too many ...'s".
;(define-syntax switchq
;  (syntax-rules (else)
;    ((switchq compare key clause ...)
;     (letrec-syntax ((%switchq (syntax-rules (else)
;			         ((%switchq compare key
;					   ((key1 ...) body1 body2 ...) rest ...)
;				  (if (or (compare key 'key1) ...)
;				      (begin body1 body2 ...)
;				      (%switchq compare key rest ...)))
;
;				 ; Null body.
;				 ((%switchq compare key ((key1 ...)) rest ...)
;				  (if (not (or (compare key 'key1) ...))
;				      (%switchq compare key rest ...)))
;    
;			         ((%switchq compare key (else body ...))
;				  (begin body ...))
;
;				 ((%switchq compare key) '#f))))
;
;        (let ((k key)			 ; Eval KEY and COMPARE
;	      (c compare))		 ; just once, then call %switch.
;	   (%switchq c k clause ...)))))); C, K are vars, hence replicable.
