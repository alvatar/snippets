;;;;;;;;;;;
;; Insert into an a-list(?) (to lazy to find the real thing)
;;
;; The payload of the list for each operator is a list of
;; things insert under that operator
;;
;; a	the op
;; b	the expression
;; L	the list to add to
(defun InsertByOp (a b L)
	(if (endp L)
		`((,a (,b)))	; list dodn't have the op, start a new section
		(if (eql a (caar L))
			(cons (list (caar L) (cons b (cadar L))) (cdr L))
			(cons (car L) (InsertByOp a b (cdr L)))
		)
	)
)

#|
(insertbyop '+ 'go '((+ (hello)) (- (world))))
(insertbyop '- 'go '((+ (hello)) (- (world))))
(insertbyop '/ 'go '((+ (hello)) (- (world))))
(insertbyop '* 'go '())
(insertbyop '% 'go (insertbyop '* 'go '()))
|#



;;;;;;;;;;;
;; Sort a list of expressions into an a-list by the 
;  top operator of each expression
;;
;; toSort	a list of expressions
(defun sort_Rewrite_By_Top_Op (toSort)
	(if (endp toSort) 
		'((+) (-) (*) (/))
		(insertByOp
			(caaar toSort)
			(car toSort)
			(sort_Rewrite_By_Top_Op (cdr toSort))
		)
	)
)



;;;;;;;;;;;;;;;;;
;; test if one expression is a generalization of another.
;; true if the first expression is a trimmed version of the second
;;
;; (description inexact)
;; (implementation inexact: fails to note reuse of trimmed sub expressions)
;;
;; NOTE: Assumes well formed

(defun Generalization_Of (LHS RHS)
	(let ((ret (Generalization_Of_ LHS RHS '())))
;		(print ret)
		ret
	)

)

;;;;;;;;;;;;;;;;
;; implementation of Generalization_Of
(defun Generalization_Of_ (LHS RHS map)
;	(print "")
;	(print LHS)
;	(print RHS)
	(if (atom LHS)
		t
		(and 
			(not (atom RHS))
			(equal (car LHS) (car RHS))
			(Generalization_Of_ (cadr  LHS) (cadr  RHS) map)
			(Generalization_Of_ (caddr LHS) (caddr RHS) map)
		)
	)
)

#|
(equal nil (Generalization_Of '(+ a (- c b))       '(+ a b)))
(equal nil (Generalization_Of '(+ a (- c (/ b d))) '(+ a b)))
(equal nil (Generalization_Of '(+ a (- c (/ b d))) '(+ a (- c b)) ))
(equal t   (Generalization_Of '(+ a (- c b))       '(+ a (- c (/ b d)))))
(equal t   (Generalization_Of '(+ a b)             '(+ a (- c b)) ))
(equal t   (Generalization_Of '(+ a b)             '(+ a (- c (/ b d)))))
|#


;;;;;;;;;;;;;;;;;;;;
;; inserter function using Generalization_Of as test
;;
;; Insert A into list L such that A will be after
;; anything it is a generalization of
;;
(defun insert_By_generality (A L)
	(if (endp L)
		`(,A)
		(if (Generalization_Of (caar L) (car A))
			(cons A L)
			(cons (car L) (insert_By_generality A (cdr L)))
		)
	)
)
;(insert_By_generality '(+ a (- c b)) '((+ a (- c (/ b d))) (+ a b) ))


;;;;;;;;;;;;;;;;;;;;;
;; insertion sort using insert_By_generality
;;
(defun Sort_By_generality (L)
	(if (endp L)
		'()
		(insert_By_generality (car L) (Sort_By_generality (cdr L)) )
	)
)

#|
(sort_By_generality 
	'(
		((+ a                 b           ) 're)
		((+ a (->    c    (/> b     d   ))) 'rc)
		((+ a (-> (+ c a)     b          )) 'rb)
		((+ a (->    c    (/> b (*> d e)))) 'ra)
		((+ a (->    c        b          )) 'rd)
	)
)
|#