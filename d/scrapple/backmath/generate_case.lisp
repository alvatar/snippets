(load "string_stuff.lisp")


;;;;;;;;;;;;;;;;;;;
;; build a test where the outer term is "known"
;;
;; base		string
;; side		expression
(defun Test_Know_Type_Is (Root Type)
;	(print (list 'type_K type))
;	(if (atom type) t (print (list 'type_K 'op (car type))))

	(concatenate 'string 
		(if (listp type)
			(concatenate 'string "is(" root ".Op == " (Op_name (car type)) ") && ")
			""
		)
		(if
			(atom type)
			(concatenate 'string "is(" root ".DefP == Defined)")
			(concatenate 'string
				(Test_Know_Type_Is (concatenate 'string root ".LHS") (cadr type))
				" && "
				(let ((op (car type)))
					(cond
						(
							(or
								(equal op '+)
								(equal op '-)
								(equal op '*)
								(equal op '/)
							)
							(Test_Know_Type_Is (concatenate 'string root ".RHS") (caddr type))
						)
						(
							(or
								(equal op '+>)
								(equal op '->)
								(equal op '*>)
								(equal op '/>)
								(equal op '-r>)
								(equal op '/r>)
							)
							(Test_UnKnow_Type_Is (concatenate 'string root ".RHS") (caddr type))
						)
						( t (format nil "%%UnKnown(~s)%%" type))			
					)
				)
			)
		)
	)
)





;;;;;;;;;;;;;;;;;;;
;; build a test where the outer term is "unknown"
;;
;; base		string
;; side		expression
(defun Test_unKnow_Type_Is (Root Type)
;	(print (list 'type_U type))
;	(if (atom type) t (print (list 'type_U 'op (car type))))

	(concatenate 'string 
		(if (listp type)
			(concatenate 'string "is(" root ".Op == " (Op_name (car type)) ") && ")
			""
		)
		(if
			(atom type)
			(concatenate 'string "is(" root ".DefP == UnDefined)")
			(concatenate 'string
				(Test_Know_Type_Is (concatenate 'string root ".LHS") (cadr type))
				" && "
				(let ((op (car type)))
					(cond
						(
							(or
								(equal op '+)
								(equal op '-)
								(equal op '*)
								(equal op '/)
							)
							(Test_Know_Type_Is (concatenate 'string root ".RHS") (caddr type))
						)
						(
							(or
								(equal op '+>)
								(equal op '->)
								(equal op '*>)
								(equal op '/>)
								(equal op '-r>)
								(equal op '/r>)
							)
							(Test_UnKnow_Type_Is (concatenate 'string root ".RHS") (caddr type))
						)
						( t (format nil "%%UnKnown(~s)%%" type))			
					)
				)
			)
		)
	)
)





;;;;;;;;;;;;;;;;;;;;;;
;; test for existance of an atom in a list
;;
(defun has (v l)
	(if (equal l nil)
		nil
		(if	(atom l)
			(equal v l)
			(or
				(equal v (car l))
				(has v (cdr l))
			)
		)
	)
)
#|
"==== has ut ===="
'(    nil         t            nil           t             nil             t)
(list (has 'a 'b) (has 'a 'a)  (has 'a '(b)) (has 'a '(a)) (has 'a '(b c)) (has 'a '(b a)) )
|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The list of implicity undefined terms
;;
(defconstant undefineds_set '(x))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root case for type test
;;
;; base		string
;; side		expression
(defun Test_Type_Is_h1 (base side)
;	(print (list 'side_h1 side))
;	(if (atom side) t (print (list 'side_h1 'LHS (cadr side))))
;	(if (atom side) t (print (list 'side_h1 'RHS (caddr side))))
	(if (atom side)
			;; for the atom as root case, test if this ID is defined as undefined
		(concatenate 'string "is(" base ".DefP == " (if (has side undefineds_set) "UnDefined" "Defined") ")")
			;; the compound root case
		(concatenate 'string
				;; test the type og op
			"is(" base ".Op == " (Op_name (car side)) ") && "
				;; LHS is always known *************************
			(Test_Know_Type_Is (concatenate 'string base ".LHS") (cadr side))
			" && "
				;; RHS might not be known
			(cond
				(
					(or
						(equal (cadr side) '+)
						(equal (cadr side) '-)
						(equal (cadr side) '*)
						(equal (cadr side) '/)
					)
					(Test_Know_Type_Is (concatenate 'string base ".RHS") (caddr side))
				)
				(
					(or
						(equal (car side) '+>)
						(equal (car side) '->)
						(equal (car side) '*>)
						(equal (car side) '/>)
						(equal (car side) '-r>)
						(equal (car side) '/r>)
					)
					(Test_UnKnow_Type_Is (concatenate 'string base ".RHS") (caddr side))
				)
				( t "%%UnKnown%%")
			)
		)
	)
)



;;;;;;;;;;;;;;;;;;
;; build a test to match a type pettern
;;
(defun Test_Type_Is (Type)
;	(print (list 'Test_Type_Is Type))
;	(print (list 'Test_Type_Is 'lhs (cadr Type)))
;	(print (list 'Test_Type_Is 'rhs (caddr Type)))
	(if (atom type)
		"/+ This is somewhat erroneous +/"
		(concatenate 'string 
			nl
			"		"
			(Test_Type_Is_h1 "T" (cadr type))
			" &&" nl
			"		"
			(Test_Type_Is_h1 "V" (caddr type))
		)
	)
)

#|
"===UNITTEST==="
(and 
	(print (Test_Type_Is '(+ a b)))
	(print (Test_Type_Is '(+ (-> a c) b)))
	'()
)
|#



;;;;;;;;;;;;;;;;;;;;;;;;
;; Find an instance of Exp in Source
;;
;; Exp		the thing to find
;; Source	the expression to search
;; LHS/RHS	the strings to add for the LHS and RHS branches
(defun Find_In (Exp Source LHS RHS)
	(cond
		((equal Exp Source) "")
		((not (listp Source)) nil)
		(
			t
;			(print `(source ,source Exp ,Exp))
			(let ((ret-l (Find_In Exp (cadr Source) ".LHS" ".RHS")))
				(if	ret-l
					(concatenate 'string LHS ret-l)
					(let ((ret-r (Find_In Exp (caddr Source) ".LHS" ".RHS")))
						(if	ret-r
							(concatenate 'string RHS ret-r)
							'nil
						)
					)
				)
			)
		)
	)
)
#|
(find_in 'a 'a "L" "R")
(find_in 'a '(+ a b) "L" "R")
(find_in 'a '(+ (/ c a) b) "L" "R")
(find_in 'a '(+ (/ c (* a d)) b) "L" "R")
|#


;;;;;;;;;;;;;;;;;;;;
;; find name of all instance of Exp
(defun Find_In_all (Exp Source)
	(cond
		((not (listp Source)) nil)
		(t
			(append
				(Find_In_all_ Exp (cadr Source)  "T")
				(Find_In_all_ Exp (caddr Source) "V")
			)
		)
	)
)
#|
(find_in_all 'a 'a "B")
(find_in_all 'a '(+ a b) "B")
(find_in_all 'a '(+ (/ c a) b) "B")
(find_in_all 'a '(+ (/ c (* a d)) b) "B")
(find_in_all 'c '(+ (/ c (* a (-> d c))) (- b c)) "B")
(Find_In_all 'c '(+ (+> a c) (+> b c)) "R")
|#

;;;;;;;;;;;;;;;;;;;
;; helper for Find_In_all
(defun Find_In_all_ (Exp Source Path)
	(cond
		((equal Exp Source) (list (concatenate 'string path "")))
		((not (listp Source)) nil)
		(t
			(append
				(Find_In_all_ Exp (cadr Source)  (concatenate 'string path ".LHS"))
				(Find_In_all_ Exp (caddr Source) (concatenate 'string path ".RHS"))
			)
		)
	)
)




;;;;;;;;;;;;;;;;;;;
;; merge two lists, droping duplicates
(defun join (L1 L2)
	(cond 
		((equal nil L1) L2)
		((equal nil L2) L1)
		((equal (car L1) (car L2)) (join (cdr L1) L2))
		(t 
			(join
				(cdr L1)
				(cons (car L2) (join `(,(car L1)) (cdr L2)))
			)
		)
	)
)

#|
(join '(b c) '())
(join '() '(b c))
(join '(a) '(b c))
(join '(a b) '(b c))
(join '(b a) '(b c))
(join '(b a) '(c b))
(join '(a b) '(c b))
(join '(a b j i k l) '(c b d e f g h))
|#

;;;;;;;;;;;;;;;;;;;;;
;; get list of used IDs in an expression
;;
(defun used_ids (Exp)
	(if (atom Exp)
		`(,Exp)
		(Join
			(used_ids (cadr exp))
			(used_ids (caddr exp))
		)
	)
)
;(used_ids '(+ (+> a c) (+> b c)))


;;;;;;;;;;;;;;;;;;;;
;; extract duplicate terms in an pattern expression
;;
(defun reuse (exp)
	(let ((ret (reuse_h1 exp (used_ids exp))))
		(if (equal ret "") " /+ no repeats +/" ret)
	)
)

;;;;;;;;;;;;;;;;;;;;
;; string together same-type checks
;;
(defun reuse_h1 (exp names)
;	(print `(this ,names ,(car names) ,(cdr names)))
	(if (equal nil names)
		""
		(concatenate
			'string
			(reuse_h2 (Find_In_all (car names) exp))
			(reuse_h1 exp (cdr names))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;
;; single instance same-type check
;;
;; test first two members of names
(defun reuse_h2 (names)
	(if (or (equal nil names) (equal nil (cdr names)))
		""
		(concatenate
			'string
			" && is("
			(car names)
			" == "
			(cadr names)
			")"
		)
	)
)

;(reuse_h1 '(+ (+> a c) (+> b c)) (used_ids '(+ (+> a c) (+> b c)) ))


;;;;;;;;;;;;;;;;;;;;;;
;; Build a type based on the result side of a rule
;;
(defun Type_Construct (LHS RHS Exp Source)
;	(print `(,exp ,(Op_name (car Exp))))
	(if (atom exp)
		(if (numberp exp)
			(format nil "Value!(~s)" exp) 
			(Find_In Exp Source LHS RHS)
		)
		(concatenate
			'string
			"Op" (Op_name (car Exp)) "!("
			(type_construct LHS RHS (cadr exp) Source) ", "
			(type_construct LHS RHS (caddr exp) Source) ")" 
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the code for a List of rewright rules
;; this incues the pattern match and the result generation
(defun Test_op_and_build (O L)
	(if (endp L)
		""
		(let* 
			(	;; extrat parts of rule
				(rule (car L))
				(from (car rule))
				(to   (cadr rule))
			)
			(concatenate
				'string

				;; the conditional
				"	static if("

				;; comment/output trace
				" /+ " (format nil "~s" from) " -> " (format nil "~s" to) " +/"

				;; the form test
				(Test_Type_Is from) nl

				;; test repeated terms
				"		" (reuse from) nl

				;; end of test expreession
				"		) " nl

				;; action
				"		alias " 

				;; build type
				(Type_Construct "T" "V" to from)

				;; what to alias as
				" TypeOf" (op_name O) ";" nl

				;; otherwise
				"	else" nl

				;; try next
				(Test_op_and_build O (cdr L))
			)
		)
	)
)


;(Test_op_and_build '/ (cadr (car (sort_Rewrite_By_Top_Op (BM::Meta_Rules)))))

