(load "meta.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert an operator to it's name
;;
;; this is sued in the the construction
;; of new expression-type

(defun op_name (a)
	(cond  
		((equal a '+) "Add")
		((equal a '-) "Sub")
		((equal a '*) "Mul")
		((equal a '/) "Div")
		((equal a '+>) "AddA" )
		((equal a '->) "SubA" )
		((equal a '*>) "MulA" )
		((equal a '/>) "DivA" )
		((equal a '-r>) "SubAR" )
		((equal a '/r>) "DivAR" )
		(t "%invlaid%")
	)
)

(defun op_symb (a)
	(cond  
		((equal a '+) "+")
		((equal a '-) "-")
		((equal a '*) "*")
		((equal a '/) "/")
		((equal a '+>) "+>")
		((equal a '->) "->")
		((equal a '*>) "*>")
		((equal a '/>) "/>")
		((equal a '-r>) "-r>")
		((equal a '/r>) "/r>")
		(t "%invlaid%")
	)
)

;;; ?New-line constant
(DEFCONSTANT nl "
")
