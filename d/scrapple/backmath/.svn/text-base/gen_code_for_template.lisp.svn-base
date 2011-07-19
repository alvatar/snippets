(load "string_stuff.lisp")
(load "rule_sorting.lisp")
(load "generate_case.lisp")

;;; load rule set
(load "meta.lisp")

;;;;;;;;;;; set this to true to make backmath emit compile time debuggin info
(DEFCONSTANT verbose nil)

;; Convert (OP (rules...)) into a text implementation
;
(defun Code_Single_Op (L)
	(let
		(
			(opis (car L))
			(sorted (Sort_By_generality (cadr L)))
		)
		(concatenate
			'string
			"template TypeOf" (op_name opis) "(T, V)"  nl 
			"{"  nl 
			(Test_op_and_build opis sorted)
			"		{}" nl
			"	static if(!is(TypeOf" (op_name opis) "))" nl
			"		static assert(false, `unusable types used for " (op_name opis) ": (" (op_symb opis) " ` ~~ T.LispOf ~~ ` `~~ V.LispOf ~~ `)` );"  nl 
			(if verbose
				(concatenate
					'string
					"	else" nl
					"		pragma(msg, `>> " (op_name opis) ": (" (op_symb opis) " ` ~~ T.LispOf ~~ ` `~~ V.LispOf ~~ `)"
					" => `"
					" ~~ TypeOf" (op_name opis) ".LispOf);"  nl 
				)
				""
			)
			"}"  nl 
		)
	)
)

;; Map and concantinate Code_Single_Op
; 
(defun Code_Ops (L)
	(if (endp L)
		""
		(concatenate
			'string
			(Code_Single_Op (car L))
			(Code_Ops (cdr L))
		)
	)
)


;; Generate code string from sorted rewriet rule list
;
(defun gen_Code_For_Template (L)
	(concatenate
		'string
		(Code_Ops (sort_Rewrite_By_Top_Op L ))
		 nl 
	)
)


;;;; generate code and ump to file
(with-open-file (s "generated_rules.d" :direction :output :if-exists :supersede)
	(format s 
		(gen_Code_For_Template 
			Meta_Rules
		;	'(
		;		((- (-> b x) a)  (+> (- a b) x))
		;		((- x a)  (+> a x))
		;		((+ x a)  (-> a x))
		;	)
		)
	)
)

'(CODE_GEN_DONE)