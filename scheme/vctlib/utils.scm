;;; -*-scheme-*-
;;; utils.scm
;;;
;;; General usage fonctions for list and vector manipulation and basic
;;; mathematical functions. Some of those functions are part of srfis
;;; 
;;; 
;;; Copyright 2005-2007 Pierre-Alexandre Fournier
;;; pafournier@gmail.com
;;; All rights reserved.
;;; 

;;; (include "srfi-1.scm")

(declare 
 (standard-bindings)
 (extended-bindings)
 (inline)
 (inlining-limit 100)
 (constant-fold)
 (lambda-lift)  
 (not safe) ;;WARNING: gives you a 40% speed-up, but you should debug
            ;;before
 (block))

;;;;
;constants
(define pi (* 4 (atan 1)))

; 2*pi
(define  2pi (* +2 pi))
(define -2pi (* -2 pi))
(define  -pi (- pi))

;increments
(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

;infinity (redefined for portability)
(define +infinity +inf.0)
(define -infinity -inf.0)

;; Booleans predicates
(define (false? x)  (eq? #f x))
(define (true?  x)  (not (false? x)))

(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))

;; logical applys (for long boolean lists)
(define (apply-and lst)
  (cond ((null? lst)
	 #t)
	(else
	 (and (car lst)
	      (apply-and (cdr lst))))))

(define (apply-or lst)
  (cond ((null? lst)
	 #f)
	(else
	 (or (car lst)
	     (apply-or (cdr lst))))))

(define (apply= lst)
  (cond ((length< lst 2)
	 #t)
	(else
	 (and (= (car lst) (cadr lst))
	      (apply= (cdr lst))))))

;;; better than logical applys
(define (true-list? lst)
  (foldr (lambda (x y) (and x y)) #t lst))

(define (true-in-list? lst)
  (foldr (lambda (x y) (or x y)) #t lst))

(define (cons-right elem lst)
  (reverse (cons elem (reverse lst))))

(define-macro (cons-right! elem lst)
  `(begin
     (set! ,lst (reverse (cons ,elem (reverse ,lst))))
     ,lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maths

;;; logarithms logb: log base b
(define (logb x b)  (/ (log x) (log b)))
(define (log10 x)   (/ (log x) (log 10)))
(define (lg x)      (/ (log x) (log 2)))
(define (10log10 x) (* 10 (log10 x)))
(define (20log10 x) (* 20 (log10 x)))

;;; sigmoid function
(define (logsig x)  (/ 1 (+ 1 (exp (- x)))))

;;; sign function
(define (sign x) (if (>= x 0) +1 -1))

;;; inverse
(define (inverse x)  (/ 1 x))

;;; square
(define (square x)  (* x x))

;;;sinus cardinal
(define (sinc x)    
  (if (= x 0) 
      1
      (/ (sin (* pi x)) (* pi x))))

;;; exact rounding
(define (exact-round x)   (inexact->exact (round x)))
(define (exact-floor x)   (inexact->exact (floor x)))
(define (exact-ceiling x) (inexact->exact (ceiling x)))
(define ->inexact exact->inexact)
(define ->exact   inexact->exact)
;;; float modulo
(define (fmodulo x y)
  (* y (- (/ x y) (floor (/ x y)))))

;;; floor relative to multiples of a float (complements fmodulo:
;;; ffloor+fmodulo = x)
(define (ffloor x y)   (* y (floor (/ x y))))
;;; fceiling = ffloor + y
(define (fceiling x y) (* y (ceiling (/ x y))))

(define (pos-lin x) 
  (cond ((> x 0) x)
	(else   
	 (if (inexact? x) 0. 0))))

(define (fmodulo-pi  x) (fmodulo x  pi))
(define (fmodulo-2pi x) (fmodulo x 2pi))

;;; takes a phase and returns a value between -pi and pi
(define (wrap-pi phase)
  (let ((p (fmodulo phase 2pi)))
    (if (> p pi)
	(- p 2pi)
	p)))

;;; lambda float modulo
(define (lambda-fmodulo y)
  (lambda (x)
    (* y 
       (- (/ x y)
	  (floor (/ x y))))))

;;; find coordinates (x . y) of the maximum of the quadratic function
;;; passing through (-1, y0) (0, y1) and (1, y2)
(define (quad-max y0 y1 y2)
  (let* ((c y1)
	 (b    (/ (- y2 y0) 2))
	 (a (- (/ (+ y2 y0) 2) y1))
	 (x (/ (- b) 
	       (* 2 a)))
	 (y (+ (/ (- (square b))
		  (* 4 a))
	       c)))
    (cons x y)))


;sinus with amplitude 1 = -3 dB0v.
;nrg = mean square (or variance, because we always remove offset)
;rms = root mean square
;db  = dB0v
;nrg->db
(define (nrg->db nrg)  (* 10 (log10 nrg)))
;db->nrg
(define (db->nrg db)   (expt 10 (/ db 10)))
;rms->db
(define (rms->db rms)  (* 20 (log10 rms)))
;db->rms
(define (db->rms db)   (expt 10 (/ db 20)))

(define (mean-db lst)  (nrg->db (mean (map db->nrg lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists operations

;;;; defined in srfi-1
;(define (filter pred? lst)
;  (cond ((null? lst)
;	 '())
;	((pred? (car lst))
;	 (cons (car lst) (filter pred? (cdr lst))))
;	(else
;	 (filter pred? (cdr lst)))))
;
;;;; defined in srfi-1
;(define (filter-map f lst)
;  (filter (lambda (x) (not (equal? #f x)))
;	  (map f lst)))
;
;;;; defined in srfi-1
;(define (remove pred? lst)
;  (let ((npred? (lambda (x) (not (pred? x)))))
;    (filter npred? lst)))
;
;;;; defined in srfi-1
;(define (take lst n)
;  (let loop ((acc '())
;	     (l   lst)
;	     (n   n))
;    (cond ((null? l)
;	   (reverse acc))
;	  ((< n 1)
;	   (reverse acc))
;	  (else
;	   (loop (cons (car l) acc)
;		 (cdr l)
;		 (- n 1))))))
;
;;;; defined in srfi-1
;(define (take-right lst n)
;  (reverse (take (reverse lst) n)))
;
;;;; defined in srfi-1
;(define (drop lst n)
;  (cond ((null? lst)
;	 '())
;	((= n 0)
;	 lst)
;	(else
;	 (drop (cdr lst) (- n 1)))))
;
;;;; defined in srfi-1
;(define (drop-right lst n)
;  (reverse (drop (reverse lst) n)))

(define (remove-until pred? lst)
  (cond ((null? lst)
	 lst)
	((pred? (car lst))
	 lst)
	(else
	 (remove-until pred? (cdr lst)))))

(define (filter-until pred? lst)
  (let loop ((lst lst)
	     (acc '()))
    (cond ((null? lst)
	   (reverse acc))
	  ((pred? (car lst))
	   (reverse acc))
	  (else
	   (loop (cdr lst) (cons (car lst) acc))))))

;;;you should avoid nth-nth or list-ref: use vectors instead
(define (nth-nth lst i j)
  (list-ref (list-ref lst i) j))

(define (randomth lst)
  (let ((n (random-integer (length lst))))
    (list-ref lst n)))

;;; randomize elements order
(define (vector-randomize v)
  (let* ((len (vector-length v))
	 (rvect (make-vector len)))
    (let loop ((len   len)
	       (v  v))
      (cond ((= 0 len)
	     rvect)
	    (else
	     (let* ((len-1 (- len 1))
		    (n     (random-integer len))
		    (elem  (vector-ref v n))
		    (new-vect (vector-append (vector-subseq v 0 (- n 1))
					     (vector-subseq v (+ n 1) len-1))))
	       (vector-set! rvect len-1 elem)
	       (loop len-1
		     new-vect)))))))

;;; randomize elements order
(define (list-randomize lst)
  (vector->list (vector-randomize (list->vector lst))))

;;; shortcuts (part of srfi-1)
;(define (first   lst) (list-ref lst 0))
;(define (second  lst) (list-ref lst 1))
;(define (third   lst) (list-ref lst 2))
;(define (fourth  lst) (list-ref lst 3))
;(define (fifth   lst) (list-ref lst 4))
;(define (sixth   lst) (list-ref lst 5))
;(define (seventh lst) (list-ref lst 6))
;(define (eighth  lst) (list-ref lst 7))
;(define (ninth   lst) (list-ref lst 8))
;(define (tenth   lst) (list-ref lst 9))
;;;; other shortcuts
;(define (take1 lst) (take lst 1))
;(define (take2 lst) (take lst 2))
;(define (take3 lst) (take lst 3))
;(define (take4 lst) (take lst 4))
;(define (take5 lst) (take lst 5))
;(define (take6 lst) (take lst 6))

;(define (last lst) (car (reverse lst)))
(define (last-n lst n)
  (reverse (take (reverse lst) n)))

(define (not-last lst)
  (if (null? lst)
      '()
      (reverse (cdr (reverse lst)))))

(define (list-subseq lst start . end)
  (let ((end (if (not (null? end)) (car end) #f)))
    (if end
	(take (drop lst start) (- end start -1))
	(drop lst start))))

;;; in srfi-1
(define (chop lst k)
  (cond ((null? lst)
	 '())
	((> (length lst) k)
	 (cons (take lst k) (chop (drop lst k) k)))
	(else
	 (list lst))))

(define (chop-overlap lst k len-overlap)
  (cond ((null? lst)
	 '())
	((> (length lst) (+ k 1 len-overlap))
	 (cons (take lst (+ k 1 len-overlap))
	       (chop-overlap (drop lst k) k len-overlap)))
	(else
	 (list lst))))

;;; chop and returns a list of blocks
(define (chop-accumulate pred? lst)
  (let loop ((acc  '())
	     (blks '())
	     (lst  lst))
    (cond ((null? lst)
	   (if (null? acc)
	       (reverse blks)
	       (reverse (cons (reverse acc) blks))))
	  ((pred? (car lst))
	   (loop (list (car lst))
		 (if (null? acc)
		     blks
		     (cons (reverse acc)
			   blks))
		 (cdr lst)))
	  (else
	   (loop (cons (car lst) acc)
		 blks
		 (cdr lst))))))

;;; useful with chop-accumulate
(define (progressive-comparator lst)
  (let ((lst lst))
    (lambda (x)
      (cond ((null? lst) #f)
	    (else
	     (let ((result (> x (car lst))))
	       (cond (result
		      (set! lst (cdr lst))
		      #t)
		     (else #f))))))))
    
;;; f takes two parameters: split the list between two elements if f
;;; is true for those two elements
(define (list-split f lst)
  (let loop ((lst  lst)
	     (blks '()))
    (cond 
     ((null? lst)
	   (reverse blks))
	  (else
	   (let loop-blk ((lst lst)
		       (blk '()))
	     (cond ((or (length< lst 2)
			(f (car lst) (cadr lst)))
		    (loop (cdr lst) 
			  (cons (reverse (cons (car lst) blk))
				blks)))
		   (else
		    (loop-blk (cdr lst) 
			   (cons (car lst) blk)))))))))

;;; like srfi-1's split-at but for a list of splits
(define (splits-at lst splits)
  (cond ((or (null? splits)
	     (null? lst))
	 lst)
	(else
	 (let ((splits (remove (lambda (x) (<= x 0))
			       (sort splits <))))
	   (let loop ((blks   '())
		      (lst    lst)
		      (splits (cons (car splits)
				    (delta splits))))
	     (cond ((or (null? splits)
			(> (first splits) (length lst)))
		    (reverse (cons lst blks)))
		   ((null? lst)
		    (reverse blks))
		   (else
		    (let* ((new-blk (take lst (first splits))))
		      (loop (cons new-blk blks)
			    (drop lst (first splits))
			    (cdr splits))))))))))


;;; makes k lists of (almost) equal length
(define (list-segment lst k)
  (let* ((len (length lst))
	 (n   (exact-floor (/ len k))))
    (cond ((< k 2)
	     (list lst))
	  ((null? lst)
	   (list lst))
	  (else
	   (cons (take lst n)
		 (list-segment (drop lst n)
			       (- k 1)))))))

(define (one-elem-each-n lst n)
  (let loop ((acc (list (car lst)))
	     (l   (cdr lst)))
    (cond ((length< l n)
	   (reverse acc))
	  (else
	   (loop (cons (list-ref l (- n 1)) acc)
		 (drop l n))))))

;;; map-map: (map f x) on each element of a list
(define (map-map f lst)
  (map (lambda (x) (map f x)) lst))

;;; map at deepness n (map^n f lst 2) == (map-map f lst)
(define (map^n f lst n)
  (if (< n 2)
      (map f lst)
      (map (lambda (x) (map^n f x (- n 1))) lst)))

(define (map-on-cdr f lst)
  (map cons 
       (map car lst)
       (map f (map cdr lst))))

;;; analog to vct-outer-product
(define (outer-map f lst1 lst2)
  (map (lambda (elem1)
	 (map (lambda (elem2)
		(f elem1 elem2))
	      lst2))
       lst1))

;;flatten : list -> list 
;;turn lists-of-lists or pairs into just one "flat" list. 
(define (flatten tree)
  (cond ((null? tree) 
	 '())
	((not (pair? tree))
	 (list tree))
	((or (list? (car tree)) (pair? (car tree)))
	 (append (flatten (car tree)) (flatten (cdr tree))))
	(else 
	 (cons (car tree) (flatten (cdr tree))))))

;;; fold right
(define (foldr f base lst)
  (let loop ((acc base)
	     (l   (reverse lst)))
    (if (null? l)
	acc
	(loop (f (car l) acc)
	      (cdr l)))))

;;; fold left
(define (foldl f base lst)
  (if (null? lst)
      base
      (foldl f (f base (car lst)) (cdr lst))))

(define filter-if filter)
; (filter-if pred lst) 
;  (cond
;   ((null? lst) '())
;   ((pred (car lst))
;    (cons (car lst) (filter-if pred (cdr lst))))
;   (else
;    (filter-if pred (cdr lst)))))

(define (remove-if pred lst)
  (filter-if (lambda (v) (not (pred v))) lst))

(define (remove-nth lst n)
  (append (take lst n)
	  (drop lst (+ n 1))))

;;; remove contiguous element that are equal according to function f.
;;; ex: (remove-doubles = '(0 0 1 2 2 3 4 5 5)) -> (0 1 2 3 4 5)
(define (remove-doubles f lst)
  (let loop ((lst1 (cdr lst))
	     (lst2 (list (car lst))))
    (cond ((null? lst1)
	   (reverse lst2))
	  (else
	   (if (f (car lst1) (car lst2))
	       (loop (cdr lst1) lst2)
	       (loop (cdr lst1) (cons (car lst1) lst2)))))))

;;; f is a (lambda (x-1 x x+1) ... ) function: if #t, x is
;;; kept. Warning: it always flushes last elements.
(define (filter-in-context lst f)
  (define (filter acc lst x-1)
    (cond ((< (length lst) 2)
	   (reverse acc))
	  ((f x-1 (car lst) (cadr lst))
	   (filter (cons (car lst) acc)
		   (cdr lst)
		   (car lst)))
	  (else
	   (filter acc
		   (cdr lst)
		   (car lst)))))
  (cond ((null? lst)
	 '())
	(else
	 (filter '() (cdr lst) (car lst)))))

(define (remove-in-context lst f)
  (filter-in-context lst (lambda (x-1 x x+1) (not (f x-1 x x+1)))))

;;; remove only one element according to f is a (lambda (x-1 x x+1)
;;; ... )
(define (remove-most-in-context lst f)
  (let* ((criteria (map f 
			(drop-right lst 2)
			(drop (drop-right lst 1) 1)
			(drop lst 2)))
	 (index (find-max-pos criteria)))
    (remove-nth lst (+ 1 index))))
    

;;; find position of pred element of lst, start by comparing with
;;; init. In case of equality, last element (pred is <, >, = or any
;;; function that can compare to elements.
(define (find-pos pred init lst)
  (cond ((null? lst)
	 #f)
	(else
	 (let loop ((pos 0)
		    (curpos 0)
		    (max init)
		    (l lst))
	   (cond ((null? l)
		  pos)
		 ((pred max (car l))
		  (loop pos (+ 1 curpos) max (cdr l)))
		 (else
		  (loop curpos (+ 1 curpos) (car l) (cdr l))))))))

(define (find-max-pos lst)  (find-pos > -infinity lst))
(define (find-min-pos lst)  (find-pos < +infinity lst))

(define (apply-min lst)
  (cond ((null? lst)     #f)
	((length= lst 1) (car lst))
	(else
	 (min (car lst)
	      (apply-min (cdr lst))))))

(define (apply-max lst)
  (cond ((null? lst)     #f)
	((length= lst 1) (car lst))
	(else
	 (max (car lst)
	      (apply-max (cdr lst))))))

(define (get-most pred f lst)
  (cond ((length< lst 2)
	 lst)
	(else
	 (let ((f-lst (map cons (map f lst) lst)))
	   (let loop ((f-lst (cdr f-lst))
		      (best  (car f-lst)))
	     (cond ((null? f-lst)
		    (cdr best))
		   (else
		    (if (pred (car best) (caar f-lst))
			(loop (cdr f-lst) best)
			(loop (cdr f-lst) (car f-lst))))))))))

(define (get-max f lst) (get-most > f lst))
(define (get-min f lst) (get-most < f lst))

;;; returns position of first element of lst that satisfies function
;;; test
(define (find-first test lst)
  (cond ((null? lst)
	 '())
	(else
	 (let loop ((l lst)
		    (n 0))
	   (if (test (car l))
	       n
	       (loop (cdr l) (+ n 1)))))))

;;; add an index before sorting
(define (sort-index lst)
  (let ((indexed-lst 
	 (map cons 
	      (iota (length lst))
	      lst)))
    (map car 
	 (sort indexed-lst 
	       (lambda (x y) (< (cdr x) (cdr y)))))))

;;; shortcuts
(define (car= x y)   (= (car x)  (car y)))
(define (car> x y)   (> (car x)  (car y)))
(define (car< x y)   (< (car x)  (car y)))
(define (cadr= x y)  (= (cadr x) (cadr y)))
(define (cadr> x y)  (> (cadr x) (cadr y)))
(define (cadr< x y)  (< (cadr x) (cadr y)))
(define (cdr= x y)   (= (cdr x)  (cdr y)))
(define (cdr> x y)   (> (cdr x)  (cdr y)))
(define (cdr< x y)   (< (cdr x)  (cdr y)))

;;; list length utilities
(define (length> lst n)
  (cond ((null? lst)
	 #f)
	((< n 1)
	 #t)
	(else
	 (length> (cdr lst) (- n 1)))))

(define (length< lst n)
  (cond ((< n 1)
	 #f)
	((null? lst)
	 #t)
	(else
	 (length< (cdr lst) (- n 1)))))

(define (length= lst n)
  (cond ((< n 0)
	 #f)
	((and (null? lst) (< n 1))
	 #t)
	((null? lst)
	 #f)
	(else
	 (length= (cdr lst) (- n 1)))))

;;longest sequence of equal numbers (not optimized)
(define (longest-number-sequence lst)
  (let loop ((i 0)
	     (best-seq-start 0)
	     (best-seq-len   0)
	     (seq-start 0)
	     (seq-len   0)
	     (lst lst))
    (cond ((length< lst 2)
	   (cons best-seq-start best-seq-len))
	  ((= (car lst) (cadr lst))
	   (loop (+ i 1) 
		 best-seq-start best-seq-len 
		 seq-start      (+ seq-len 1) 
		 (cdr lst)))
	  (else
	   (if (> seq-len best-seq-len)
	       (loop (+ i 1) 
		     seq-start (+ seq-len 1)
		     (+ i 1)   0
		     (cdr lst))
	       (loop (+ i 1) 
		     best-seq-start best-seq-len 
		     (+ i 1)        0
		     (cdr lst)))))))

;;longest sequence for which f is true
(define (longest-sequence lst f)
  (let loop ((i 0)         (best-seq-start 0) (best-seq-len   0)
	     (seq-start 0) (lst-2 lst))
    (cond ((null? lst-2)
	   (list-subseq lst best-seq-start (+ best-seq-len best-seq-start -1)))
	  ((f (car lst-2))
	   (loop (+ i 1)   best-seq-start best-seq-len 
		 seq-start (cdr lst-2)))
	  (else
	   (let ((len (- i seq-start)))
	     (if (> len best-seq-len)
		 (loop (+ i 1) seq-start len
		       (+ i 1) (cdr lst-2))
		 (loop (+ i 1) best-seq-start best-seq-len 
		       (+ i 1) (cdr lst-2))))))))

(define (first-n-sequence-start lst f n)
  (let loop ((i 0)         
	     (seq-start 0)
	     (lst lst))
    (cond ((>= (- i seq-start) n)
	   seq-start)
	  ((null? lst)
	   #f)
	  ((f (car lst))
	   (loop (+ i 1)
		 seq-start
		 (cdr lst)))
	  (else
	   (loop (+ i 1)
		 (+ i 1)
		 (cdr lst))))))

;divide all element to make sum = 1
(define (list-normalize lst)
  (let ((s (sum lst)))
    (map (lambda (x)
	   (/ x s))
	 lst)))

;convert to string
(define (stringify stuff)
  (with-output-to-string
    (lambda () (display stuff))))

(define (conc . something)
  (with-output-to-string "" (lambda () (display something))))

(define (float->string x precision)
  (let* ((k (expt 10 precision))
	 (y (* k x)))
    (conc (/ (round y) k))))

(define (string-intersperse lst str)
  (fold-right 
   conc "" 
   (flatten
    (append
     (map (lambda (s) (conc s str))
	  (drop-right lst 1))
     (list (last lst))))))

;;transpose a list of list
(define (list-transpose lst)
  (let loop ((lst  lst) 
	     (tlst '()))
    (cond ((null? (car lst))
	   (reverse tlst))
	  (else
	   (loop (map cdr lst) 
		 (cons (map car lst) tlst))))))

;;;switch car-cdr of a list of pairs
(define (switch-car-cdr-pairs lst)
  (cond ((null? lst)
	 '())
	(else
	 (let ((pair (car lst)))
	   (cons (list (cadr pair)
		       (car pair))
		 (switch-car-cdr-pairs (cdr lst)))))))

;;;display elements of a list without the parenthesis
(define (list-display lst)
  (cond ((null? lst)
	 (display "\n"))
	(else
	 (display (car lst))
	 (display " ")
	 (list-display (cdr lst)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math, Statistics on lists

;scale
(define (scale lst k)  
  (map (lambda (x) (* k x)) lst))

;offset
(define (offset lst k)  
  (map (lambda (x) (+ k x)) lst))

;sum of squares
(define (sum-of-squares lst)
  (sum (map square lst)))

(define (energy-diff lst1 lst2)
  (sum (map square (map - lst1 lst2))))

;; first derivative of a list
(define (delta lst)
  (cond ((null? lst)
	 '())
	((length= lst 1)
	 '())
	(else
	 (cons (- (cadr lst) (car lst))
	       (delta (cdr lst))))))

(define (delta-f f lst)
  (cond ((null? lst)
	 '())
	((length= lst 1)
	 '())
	(else
	 (cons (f (cadr lst) (car lst))
	       (delta-f f (cdr lst))))))

;similar to progressive sum
(define (integral lst)
  (let loop ((lst lst)
	     (l2 '()))
    (cond ((null?   lst)
	   (reverse l2))
	  (else
	   (loop (cdr lst)
		 (cons (+ (if (null? l2) 0 (car l2) )
			  (car lst)) l2))))))
	 
(define (threshold lst thres)
  (map (lambda (x)
	 (>= x thres))
       lst))

(define (heaviside x0)
  (lambda (x)
    (cond ((> x0 x)
	   0)
	  ((< x0 x)
	   1)
	  ((= x0 x)
	   0.5))))

;;; sum of all lst elements
(define (sum lst)
  (foldr + 0 lst))

;;; product of all lst elements
(define (product lst)
  (foldr * 1 lst))

;;; mean
(define (mean lst)  (/ (sum lst) (length lst)))

;;; median
(define (median lst)
  (let ((len (length lst)))
    (list-ref (sort lst <) 
	      (exact-floor (/ len 2)))))

;;; geometric-mean
(define (geometric-mean lst)
  (expt (product lst) 
	(/ 1 (length lst))))

;;; sliding function
(define (sliding-f f lst width)
  (let* ((len     (length lst))
	 (new-len (- len width)))
    (let loop ((i 0)
	       (lst lst)
	       (new-lst '()))
      (cond ((>= i new-len)
	     (reverse new-lst))
	    (else
	     (loop (+ i 1)
		   (cdr lst)
		   (cons (f (take lst width))
			 new-lst)))))))

(define (sliding-median lst width)
  (sliding-f median lst width))

(define (sliding-mean lst width)
  (sliding-f mean lst width))

;;; variance
(define (variance lst)
  (let ((m (mean lst)))
    (mean (map (lambda (x) 
		 (square (- x m)))
	       lst))))

;;; std
(define (std lst) (sqrt (variance lst)))

;;; root-mean-square (WARNING: this doesn't remove the average first)
(define (rms lst) (sqrt (sum-of-squares lst)))

(define (correlation x y)
  (define (mean-dev x)
    (map (lambda (a) (- a (mean x))) 
	 x))
  (sum (map * 
	    (mean-dev x) 
	    (mean-dev y))))

(define (correlation-coeff x y)
  (let ((sxx (correlation x x))
	(syy (correlation y y))
	(sxy (correlation x y)))
    (if (or (= 0 sxx)
	    (= 0 syy))
	0;Division by zero
	(/ (square sxy)
	   (* sxx syy)))))


;;; factorial
(define (factorial n)    
  (cond ((< n 2)
	 1)
	(else
	 (* n (factorial (- n 1))))))

;;; faster than using factorials
(define (binomial n k)
  (cond ((= k 0) 1)
	((= n 0) 0)
	(else
	 (* (/ n k)
	    (binomial2 (- n 1) (- k 1))))))

;;; all binomial possible outcomes
(define (binomials n k)

  (define (get-combinations acc)
    (let ((local-k (- k (length acc)))
	  (start   (first acc)))
      (cond ((<= local-k 0)
	     (list->vector (reverse acc)))
	    (else
	     (let ((next-x (iota (- n local-k start -1)
				 (+ start 1))))
	       (map (lambda (x) (get-combinations (cons x acc)))
		    next-x))))))
  
  (cond ((or (< k 0)
	     (> k n)) 
	 '())
	((= k 0)
	 '(()))
	(else
	 (map vector->list
	      (flatten
	       (map get-combinations
		    (map list (iota (- n k -1) 1))))))))


;(define (test-bin n k)
;  (= (length (binomials n k))
;     (bin n k)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List Initialization
;zeros: a list of n zeros

(define (zeros n) (make-list n 0))
(define (ones n)  (make-list n 1))

;;; bad version, use srfi-1 instead
;(define (iota n) 
;  (from-to 0 (- n 1)))

(define (from-to a b)
  (from-to-step a b 1))

;;; srfi-1's iota: rename
(define (from-to-step a b step)
  (let loop ((i a)
	     (acc '()))
    (if (or (and (>= b a) (> i b))
	    (and (<= b a) (< i b)))
	(reverse acc)
	(loop (+ i step)
	      (cons i acc)))))

(define (list-random-real n)
  (let loop ((i n)
	     (acc '()))
    (cond ((<= i 0)
	   acc)
	  (else
	   (loop (- i 1)
		 (cons (random-real) acc))))))

(define (list-random-integer n max)
  (let loop ((i n)
	     (acc '()))
    (cond ((<= i 0)
	   acc)
	  (else
	   (loop (- i 1)
		 (cons (random-integer max) acc))))))


(define (reset-even lst)
  (cond ((null? lst)
	 '())
	((length= lst 1)
	 (list (car lst)))
	(else
	 (append (list (car lst)
		       0)
		 (reset-even (cdr (cdr lst)))))))

(define (reset-odd lst)
    (cond ((null? lst)
	   '())
	  ((length= lst 1)
	   '(0))
	  (else
	   (append (list 0
			 (cadr lst))
		   (reset-odd (cdr (cdr lst)))))))

(define (list-pad lst pad-len)
  (define (pad-before lst pad-len)
    (let loop ((i 0)
	       (l lst))
      (if (< i pad-len)
	  (loop (+ i 1)
		(cons (car lst) l))
	  l)))
  (let* ((pad-start (exact-floor (/ pad-len 2)))
	 (pad-end   (- pad-len pad-start)))
    (pad-before (reverse (pad-before (reverse lst) pad-end)) pad-start)))

(define (list-pad-right lst pad-len)
  (let ((pad (last lst)))
    (append lst (make-list pad-len pad))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
(define (in-string? str pattern)
  (let ((len (string-length pattern)))
    (cond ((> len (string-length str))
	   #f)
	  ((string=? (substring str 0 len)
		     pattern)
	   #t)
	  (else
	   (in-string? (substring str 1 (string-length str)) 
		       pattern)))))

;;; chicken extras.scm like string-split
(define (string-split str . args)
  (let ((charset  (if (length> args 0) (car args)  '(#\space #\newline #\return)))
	(maxsplit (if (length> args 1) (cadr args) -1)))
    (define (char-in-charset? char)
      (not (null? 
	    (filter-if (lambda (c) (eq? char c)) charset))))
   
    (let loop-words ((str     str)
		     (words '()))
      (cond ((length= words maxsplit)
	     (reverse words))
	    (else
	     (let ((str-len (string-length str)))
	       (let loop-letters ((count 0))
		 (cond ((= count str-len)
			(reverse
			 (cons (substring str 0 str-len) words)))
		       ((char-in-charset? (string-ref str count))
			(if (= count 0)
			    (loop-words 
			     (substring str 1 str-len)		    
			     words)
			    (loop-words 
			     (substring str (+ 1 count) str-len)
			     (cons (substring str 0 count) words))))
		       (else 
			(loop-letters (+ 1 count)))))))))))

;;; this list is not complete
(define string-split-separators 
  '(#\space 
    #\newline #\return
    #\, #\.  #\!  #\?
    #\: #\; 
    #\/ #\| #\\ 
    #\$ #\@ #\$ #\% #\&
    #\' #\" #\< #\>
    #\- #\_ #\= #\+  #\*
    #\( #\) #\[ #\] #\{ #\}))

;;; naive string-reverse
(define (string-reverse str)
  (list->string (reverse (string->list str))))

;;; naive string-contains
(define (string-contains str pattern)
  (define (found? str pos pattern)
    (cond ((= 0 (string-length pattern))
	   #t)
	  ((char=? (string-ref str     pos)
		   (string-ref pattern 0))
	   (found? str
		   (+ 1 pos)
		   (substring pattern 1 (string-length pattern))))
	  (else
	   #f)))
  (let loop ((pos 0))
    (cond ((> pos (- (string-length str)
		   (string-length pattern)))
	   #f)
	  ((not (found? str pos pattern))
	   (loop (+ 1 pos)))
	  (else
	   #t))))
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;repeat (please avoid using this)
(define (repeat N expr)
  (if (= N 0) 
      '()
      (cons 
       (expr)
       (repeat (- N 1) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic I/O
;;; read-file
(define (read-file filename)
  (call-with-input-file 
      filename
    (lambda (fd) (read fd))))

(define (read-text-file filename . args)
  (cond ((not (file-exists? filename))
	 (display (string-append "File " filename " does not exists.\n")))
	(else
	 (let* ((len  (if (length> args 0) 
			  (car args)
			  (min 100000 (file-size filename))))
		(port (open-file filename))
		(text (make-u8vector len)))
	   (display len)
	   (read-subu8vector text 0 len port)
	   (display " read.\n")
	   (u8vector->string text)))))

;; write-to-file
(define (write-to-file lst filename)
  (with-output-to-file filename
    (lambda ()
      (write lst)
      #f)))

;; write-to-m-file (list of lists) for matlab
(define (write-to-m-file lst file)
  (with-output-to-file file
    (lambda ()
      (map list-display lst)
      #f)))

(define (read-lines-from-port p)
  (let ((lines '()))
    (let loop ()
      (let ((line (read-line p)))
        (cond ((not (eof-object? line))
	       (set! lines (cons line lines))
	       (loop))
	      (else
	       (reverse lines)))))))

(define (read-lines file)
  (let* ((p (open-input-file file))
         (lines (read-lines-from-port p)))
    (close-input-port p)
    lines))

;;; I/O error handling
;(define (assert-file-exists filename)
;  (cond ((not (file-exists? filename))
;	 (error (list "File: " filename " does not exist."))
;	 (break))
;	(else 
;	 #t)))

(define (assert-file-exists filename)
  (cond ((not (file-exists? filename))
	 (display (list "File: " filename " does not exist."))
	 (newline)
	 #f)
	(else 
	 #t)))
		    
;;;shell commands
(define (pwd) (read-all (open-process "pwd") read-line))
(define (ls . args)  (read-all (open-process (conc "ls " args))  read-line))

;;;size of any gambit scheme object, in bytes
(define (size obj)
  (u8vector-length (object->u8vector obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vector functions

(define (vector-iota n)
  (let ((v (make-vector n)))
    (let loop ((i 0))
      (cond ((= i n)
	     v)
	    (else
	     (vector-set! v i i)
	     (loop (+ i 1)))))))

(define (vector-reverse v)
  (list->vector (reverse (vector->list v))))

(define (vector=? v1 v2)
  (let ((len1 (vector-length v1))
	(len2 (vector-length v2)))
    (cond ((not (= len1 len2))
	   #f)
	  (else
	   (let loop ((i 0))
	     (cond ((= i len1)
		    #t)
		   (else
		    (if (= (vector-ref v1 i) (vector-ref v2 i))
			(loop (+ i 1))
			#f))))))))

;;;actually copies, unlike vector-copy that passes you a reference
(define (vector-copy-data v)
  (let* ((len   (vector-length v))
	 (new-v (make-vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (vector-set! new-v i (vector-ref v i))
	     (loop (+ i 1)))
	    (else
	     new-v)))))

(define (vector-map! f . args)
  (let* ((v1    (car args))
	 (v2    (if (length> args 1) (cadr args) #f))
	 (len   (vector-length v1)))
    (if v2 ;;check dimensions
	(let* ((len2 (vector-length v2)))
	  (if (not (= len len2))
	      (error "vector-map: vectors v1 and v2 must have same length.\n"))))
    (let loop ((i 0))
      (cond ((< i len)
	     (vector-set! v1 i 
			  (if v2
			      (f (vector-ref v1 i) 
				 (vector-ref v2 i))
			      (f (vector-ref v1 i))))
	     (loop (+ i 1)))
	    (else
	     v1)))))

;;;non mutating vector-map
(define (vector-map f . args)
  (let* ((v1    (vector-copy-data (car args)))
	 (v2    (if (length> args 1) (cadr args) #f)))
    (if v2
	(vector-map! f v1 v2)
	(vector-map! f v1))))

(define (vector-foldr f init v)
  (foldr f init (vector->list v)))

(define (vector-min v)
  (vector-foldr min +infinity v))

(define (vector-sum v)
  (vector-foldr + 0 v))

(define (vector-prod v)
  (vector-foldr * 1 v))

(define (vector-offset! v coefficient)
  (vector-map! (lambda (x) (+ coefficient x)) v))

(define (vector-subseq v start . end)
  (let* ((len      (vector-length v))
	 (start    (max start 0))
	 (end      (if (not (null? end)) 
		       (car end) 
		       (- (vector-length v) 1)))
	 (new-len  (- end start -1))
	 (max-iter (min new-len (- len start))))
    (cond ((> new-len 0)
	   (let ((new-v (make-vector new-len 0.)))
	     (if (< max-iter 1)
		 new-v
		 (do ((i 0 (+ 1 i)))
		     ((= i max-iter) new-v)
		   (vector-set! new-v i (vector-ref v (+ i start)))))))
	  (else
	   (vector)))))

(define (vector-filter-if proc v)
  (let* ((len   (vector-length v))
	 (new-v (make-vector len)))
    (let loop ((i       0)
	       (new-len 0))
      (cond ((< i len)
	     (cond ((proc (vector-ref v i))
		    (vector-set! new-v new-len (vector-ref v i))
		    (loop (+ i 1) (+ new-len 1)))
		   (else
		    (loop (+ i 1) new-len))))
	    (else
	     (vector-subseq new-v 0 (- new-len 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sorting

;;; destructive vector shell sort (slightly modified from:
;;; http://www.math.grin.edu/~stone/events/scheme-workshop/shellsort.html)
(define shell-sort!
  (lambda (v . opt)
   ;;; The computation of the initial gap size is performed by this
   ;;; auxiliary procedure:
    (define gap-size-not-exceeding
      (lambda (n)
	(let loop ((old 1) (new 4))
	  (if (< n new) old (loop new (+ (* 3 new) 1))))))

    (let ((precedes? (if (null? opt) < (car opt)))
          (len (vector-length v)))
      (let ((insert!
             (lambda (position gap)
               (let ((new (vector-ref v position)))
                 (let loop ((old-trial position)
                            (trial (- position gap)))
                   (cond ((negative? trial) ; at the left end: stop!
			  (vector-set! v old-trial new))
			 (else
			  (let ((displaced (vector-ref v trial)))
			    (cond ((precedes? new displaced)
				   (vector-set! v old-trial displaced)
				   (loop trial (- trial gap)))
				  (else
				   (vector-set! v old-trial new)))))))))))
        (do ((gap (gap-size-not-exceeding len) (quotient gap 3))) 
            ((zero? gap) v)
          (do ((position gap (+ position 1)))
              ((<= len position))
            (insert! position gap)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Merge sorting adapted from
;;; http://www.scheme.com/tspl2d/examples.html#g2335 (The Scheme
;;; Programming Language, Second Edition)
(define sort #f)
(define merge #f)
(let ()
  (define dosort
    (lambda (pred? ls n)
      (cond
        ((= n 1) (list (car ls)))
        ((= n 2) (let ((x (car ls)) (y (cadr ls)))
                   (if (pred? y x) (list y x) (list x y))))
        (else
         (let ((i (quotient n 2)))
           (domerge pred?
                    (dosort pred? ls i)
                    (dosort pred? (list-tail ls i) (- n i))))))))
  (define domerge
    (lambda (pred? l1 l2)
      (cond
        ((null? l1) l2)
        ((null? l2) l1)
        ((pred? (car l2) (car l1))
         (cons (car l2) (domerge pred? l1 (cdr l2))))
        (else (cons (car l1) (domerge pred? (cdr l1) l2))))))
  (set! sort
    (lambda (l pred?) ;;changed arguments order here
      (if (null? l) l (dosort pred? l (length l)))))
  (set! merge
    (lambda (pred? l1 l2)
      (domerge pred? l1 l2))))

;;;useful sort functions
(define (sort> lst) (sort lst >))
(define (sort< lst) (sort lst <))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sets and counting

;;; difference of two sets (lists) of numbers
(define (list-difference-numbers lst1 lst2)
  (define (is-in x lst)
    (apply-or (map (lambda (y) (= x y)) lst)))
  (define (not-in lst1 lst2)
    (filter-if (lambda (x) (not (is-in x lst2))) lst1))
  (append (not-in lst1 lst2) (not-in lst2 lst1)))

;;; intersection of two sets (lists) of numbers
(define (list-intersection-numbers lst1 lst2)
  (define (is-in x lst)
    (apply-or (map (lambda (y) (= x y)) lst)))
  (define (in lst1 lst2)
    (filter-if (lambda (x) (is-in x lst2)) lst1))
  (append (in lst1 lst2) (in lst2 lst1)))

;;; count the number of occurences of numbers in list lst
(define (count-numbers lst)
  (let* ((table (make-table init: 0)))
    (let loop ((lst lst))
      (cond ((null? lst)
	     (sort (table->list table) car<))
	    (else
	     (table-set! table (car lst) (+ 1 (table-ref table (car lst))))
	     (loop (cdr lst)))))))

;;; counts occurences in lst verifying pred
(define (count-if pred lst)
  (length (filter-if pred lst)))

;;; count the number of occurences of numbers in lst and separate them
;;; in n-bin bins of equal length
(define (histogram lst n-bin)
  (let* ((min (apply-min lst)) 
	 (max (apply-max lst))
	 (len (- max min -1))
	 (v   (make-vector n-bin 0)))
    (let loop ((lst lst))
      (cond ((null? lst) (list min max v))
	    (else
	     (let ((bin (exact-floor (* n-bin (/ (- (car lst) min) len)))))
	       (vector-set! v bin (+ 1 (vector-ref v bin)))
	       (loop (cdr lst))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binary search in a vector
;;; compare must return -1 (look before), +1 (look after), or 0 (found)
(define (binary-search v compare)
  (let* ((n (vector-length v))
	 (n/2 (floor (/ n 2))))
    (cond ((< n 1)
	   #f) ;; not found
	  ((= -1 (compare (vector-ref v n/2)))
	   (binary-search (subvector v 0 n/2) compare))
	  ((= +1 (compare (vector-ref v n/2)))
	   (binary-search (subvector v (+ n/2 1) n) compare))
	  (else
	   (vector-ref v n/2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Naive sizeof
(define (sizeof obj)
  (u8vector-length (object->u8vector obj)))

