;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This is a program for atomating parts of
;;;;; the rule generation process for BackMath
;;;;;
;;;;; For instruction on how to use it, look at
;;;;; the end of the file.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert in place a op> exp to a normal exp
;;
(defun Convert_to_exp (exp)
	(if (atom exp)
		exp
		(cond
			((equal (car exp) '+>)   `(- ,(Convert_to_exp (caddr exp)) ,(Convert_to_exp (cadr exp))))
			((equal (car exp) '->)   `(+ ,(Convert_to_exp (caddr exp)) ,(Convert_to_exp (cadr exp))))
			((equal (car exp) '*>)   `(/ ,(Convert_to_exp (caddr exp)) ,(Convert_to_exp (cadr exp))))
			((equal (car exp) '/>)   `(* ,(Convert_to_exp (caddr exp)) ,(Convert_to_exp (cadr exp))))
			((equal (car exp) '-r>)  `(- ,(Convert_to_exp (cadr exp)) ,(Convert_to_exp (caddr exp))))
			((equal (car exp) '/r>)  `(/ ,(Convert_to_exp (cadr exp)) ,(Convert_to_exp (caddr exp))))
			((equal (car exp) '+)    `(+ ,(Convert_to_exp (cadr exp)) ,(Convert_to_exp (caddr exp))))
			((equal (car exp) '-)    `(- ,(Convert_to_exp (cadr exp)) ,(Convert_to_exp (caddr exp))))
			((equal (car exp) '*)    `(* ,(Convert_to_exp (cadr exp)) ,(Convert_to_exp (caddr exp))))
			((equal (car exp) '/)    `(/ ,(Convert_to_exp (cadr exp)) ,(Convert_to_exp (caddr exp))))
		)
	)
)


(defun has (a e)
	(if (atom e)
		(equal a e)
		(or
			(has a (cadr e))
			(has a (caddr e))
		)
	)
)

(defun SetPush (un exp)
	(if (atom exp)
		exp
		(let
			(
				(op  (car   exp))
				(LHS (cadr  exp))
				(RHS (caddr exp))
			)
			(cond
				(
					(and 
						(has un LHS)
						(not (has un RHS))
					)
					(cond
						((equal op '+)    `(-> ,RHS ,(SetPush un LHS)))
						((equal op '-)    `(+> ,RHS ,(SetPush un LHS)))
						((equal op '*)    `(/> ,RHS ,(SetPush un LHS)))
						((equal op '/)    `(*> ,RHS ,(SetPush un LHS)))
					)
				)
				(
					(and 
						(not (has un LHS))
						(has un RHS)
					)
					(cond
						((equal op '+)    `(->  ,LHS ,(SetPush un RHS)))
						((equal op '-)    `(-r> ,LHS ,(SetPush un RHS)))
						((equal op '*)    `(/>  ,LHS ,(SetPush un RHS)))
						((equal op '/)    `(/r> ,LHS ,(SetPush un RHS)))
					)
				)
				(
					(and 
						(not (has un LHS))
						(not (has un RHS))
					)
					exp
				)
				(t  'ERROR)
			)
		)
	)
)


(defun prefer_neg_inv (exp)
	(cond
		((atom exp) exp)
		((endp exp) exp)
		(
			(or (equal (car exp) '+) (equal (car exp) '*))
			`(
				,(car exp) 
				,(prefer_neg_inv (cadr exp))
				,(prefer_neg_inv (caddr exp))
			)
		)
		(
			(equal (car exp) '-)
			`(
				+
				,(prefer_neg_inv (cadr exp))
				(- ,(prefer_neg_inv (caddr exp)))
			)
		)
		(
			(equal (car exp) '/)
			`(
				*
				,(prefer_neg_inv (cadr exp))
				(/ ,(prefer_neg_inv (caddr exp)))
			)
		)
		(t exp)
	)
)

#|(defun has_struct (temp act)
	(cond
		((endp tmp) t)
		((atom tmp) (not (endp act)))
		(
			t
			(and
				(not (endp act))
				(has_struct (car temp) (car act))
				(has_struct (cdr temp) (cdr act))
			)
		)
	)
)|#


(defun my_normalize (exp)
	(prefer_neg_inv exp)
)

;(my_normalize (Convert_to_exp in))

(defun partof (exp)    (and (not (atom exp)) (not (endp exp))))
(defun something (exp) (or  (atom exp) (not (endp exp))))


(defun post_ (exp)
;	(print exp)
	(cond
		((atom exp) exp)
		((endp exp) exp)
		(
			(equal '+ (car exp))
			(cond
				(
					(not (endp (cdddr exp)))
					`(+ 
						,(cadr exp)
						,(cons '+ (cddr exp))
					)
				)

				(
					(and	; (+ (- A) B)
						(not (atom exp))
						(not (endp exp))
						(equal '+ (car exp))

						(not (atom (cadr exp)))
						(not (endp (cadr exp)))
						(equal '- (caadr exp))

						(not (endp (cdadr exp)))
						(endp (cddadr exp))

						(not (endp (cddr exp)))

						(not (and
							(not (atom (caddr exp)))
							(not (endp (caddr exp)))
							(equal '- (caaddr exp))
						))

					)
					`(-
						,(caddr exp)
						,(cadadr exp)
					)
				)

				(
					(and	; (+ (- A) B)
						(not (atom exp))
						(not (endp exp))
						(equal '+ (car exp))

						(not (atom (cadr exp)))
						(not (endp (cadr exp)))
						(equal '- (caadr exp))

						(not (endp (cdadr exp)))
						(endp (cddadr exp))

						(not (endp (cddr exp)))

						(not (atom (caddr exp)))
						(not (endp (caddr exp)))
						(equal '- (caaddr exp))

					)
					`(-
						(+ 
							,(cadadr exp)
							,(car (cdaddr exp))
						)
					)
				)

				(
					(and	; (+ a (- 0 b))
						(partof    (        cdr exp) )
						(something (       cadr exp) )
							; a               (cadr exp)
						(partof    (       cddr exp) )
						(partof    (      caddr exp) )
						(something (     caaddr exp) )
							; -             (caaddr exp)
						(equal '-  (     caaddr exp) )
						(partof    (     cdaddr exp) )
						(something  (car(cdaddr exp)))
							; 0         (car(cdaddr exp))
						(equal 0    (car(cdaddr exp)))
						(something  (cdr(cdaddr exp)))
						(something (cadr(cdaddr exp)))
							; b        (cadr(cdaddr exp))
					)
					`(-	; (- a b)
						,(cadr exp)
						,(cadr(cdaddr exp))
					)
				)

				(
					t
					`(+ 
						,(post_ (cadr exp))
						,(post_ (caddr exp))
					)
				)
			)
		)
		(
			(equal '- (car exp))
			(cond

				(
					(not (endp (cdddr exp)))
					(append
						`(-
							,(cadr exp)
							(+ ,(caddr exp) ,(cadddr exp))
						)
						(cddddr exp)
					)
				)

				(	; (- (- a) b)
					(and
						(not (atom (cadr exp)))
						(not (endp (cadr exp)))
						(equal '- (caadr exp))
						(endp (cddadr exp))
					)
					; (- (+ a b))
					`(- (+ ,(cadadr exp) ,(caddr exp)))
				)
#|
				(	; (- (- a b))
					(and
						(not (atom (cadr exp)))
						(not (endp (cadr exp)))
						(equal '- (caadr exp))
						(not (endp (cddadr exp)))
						(endp (cdr (cddadr exp)))
						(endp (cddr exp))
					)
					; (- b a))
					`(- ,(caddr exp) ,(cadadr exp))
				)

				(	; (- (- a b) c)
					(and
						(not (atom (cadr exp)))
						(not (endp (cadr exp)))
						(equal '- (caadr exp))
						(not (endp (cddadr exp)))
						(endp (cdr (cddadr exp)))
						(not (endp (cddr exp)))
						(endp (cdddr exp))
					)
					; (- a (+ b c))
					`(- ,(cadadr exp) (+ ,(caddr exp) ,(caddr exp)))
				)
|#
				(
					(endp (cddr exp))
					`(- 0 ,(cadr exp))
				)

				(
					t
					`(- 
						,(post_ (cadr exp))
						,(post_ (caddr exp))
					)
				)
			)
		)
		(
			(equal '* (car exp))
			(cond
				(; (* x )
					(and
						(something (cdr exp))
						(endp (cddr exp))
					)
					(cadr exp)
				)

				(
					(not (endp (cdddr exp)))
					`(*
						,(cadr exp)
						,(cons '* (cddr exp))
					)
				)

				(
					(and	; (* (/ A) B)
						(not (atom exp))
						(not (endp exp))
						(equal '* (car exp))

						(not (atom (cadr exp)))
						(not (endp (cadr exp)))
						(equal '/ (caadr exp))

						(not (endp (cdadr exp)))
						(endp (cddadr exp))

						(not (endp (cddr exp)))

						(not (and
							(not (atom (caddr exp)))
							(not (endp (caddr exp)))
							(equal '/ (caaddr exp))
						))

					)
					`(/
						,(caddr exp)
						,(cadadr exp)
					)
				)

				(
					(and	; (* (/ A) B)
						(not (atom exp))
						(not (endp exp))
						(equal '* (car exp))

						(not (atom (cadr exp)))
						(not (endp (cadr exp)))
						(equal '/ (caadr exp))

						(not (endp (cdadr exp)))
						(endp (cddadr exp))

						(not (endp (cddr exp)))

						(not (atom (caddr exp)))
						(not (endp (caddr exp)))
						(equal '/ (caaddr exp))

					)
					`(/
						(* 
							,(cadadr exp)
							,(car (cdaddr exp))
						)
					)
				)
				(
					(and	; (* a (/ 1 b))
						(partof    (        cdr exp) )
						(something (       cadr exp) )
							; a               (cadr exp)
						(partof    (       cddr exp) )
						(partof    (      caddr exp) )
						(something (     caaddr exp) )
							; /             (caaddr exp)
						(equal '/  (     caaddr exp) )
						(partof    (     cdaddr exp) )
						(something  (car(cdaddr exp)))
							; 1         (car(cdaddr exp))
						(equal 1    (car(cdaddr exp)))
						(something  (cdr(cdaddr exp)))
						(something (cadr(cdaddr exp)))
							; b        (cadr(cdaddr exp))
					)
					`(/	; (/ a b)
						,(cadr exp)
						,(cadr(cdaddr exp))
					)
				)

				(
					t
					`(* 
						,(post_ (cadr exp))
						,(post_ (caddr exp))
					)
				)
			)
		)
		(
			(equal '/ (car exp))
			(cond

				(
					(not (endp (cdddr exp)))
					(append
						`(/
							,(cadr exp)
							(* ,(caddr exp) ,(cadddr exp))
						)
						(cddddr exp)
					)
				)

				(	; (/ (/ a) b)
					(and
						(not (atom (cadr exp)))
						(not (endp (cadr exp)))
						(equal '/ (caadr exp))
						(endp (cddadr exp))
					)
					; (/ (* a b))
					`(/ (* ,(cadadr exp) ,(caddr exp)))
				)

				(
					(endp (cddr exp))
					`(/ 1 ,(cadr exp))
				)

				(
					t
					`(/ 
						,(post_ (cadr exp))
						,(post_ (caddr exp))
					)
				)
			)
		)
		(t exp)
	)
)

(defun post (exp)
	(let
		((
			ret
			(post_ exp)
		))
		(if (equal exp ret)
			ret
			(post ret )
		)
	)
)
#|
;(defconstant tc '(* 1 2 (/ 3) (/ 4) (/ 5)))
(defconstant tc '(+ 1 2 (- 3) (- 4) (- 5)))
(post tc)
|#



(defconstant in '
;(- (*> k (-> h x)) (*> f (-> e x)))
;(- (/> K (-> H x)) (*> e x))
;(- (*> h x) (*> f (-> e x)))
;(- (*> h x) (/> e x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; #1) Copy and pastes the required form here  ;;;;;;;;;;
;;;; #2) Run the program                         ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(+ (*> h x) (*> f (-> e x)))

)


"#3) copy this expression for step 4"
(Convert_to_exp in)


(defconstant mid '

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; #4) pastes expression from step 3 here   ;;;;;;;;;
;;;; #5) rearrange so that is is of the form: ;;;;;;;;;
;;;;       (*/ A (+- X B))                    ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(* (+ (/ H) (/ F)) (+ (* x ) (/ (/ E F)(+ (/ H) (/ F)))))

)

#|
(- (* (+ X H) K) (* X E))
(- (+ (* X K) (* H K)) (* X E))
(+ (- (* X K) (* X E)) (* H K))
(+ (* X (- K E)) (* (* H K) (- K E) (/ - K E)))

(* (- K E) (+ X (* (* H K) (/ (- K E)))))
(* (- K E) (+ X (/ (* H K) (- K E))))
(* (+ X (/ (* H K) (- K E))) (- K E))
|#

(defconstant out (setpush 'x (post mid)))

;in
;mid
;(post mid)
;out

"#6) copy this expression into meta.lisp in the correct place"
`(,in ,out)

