;;
;; 
;; Simple cycle detection in a graph
;; Based on code from MLRISC
;;
;; Copyright 2007-2011 Ivan Raikov and the Okinawa Institute of Science and Technology.
;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>."))))
;;


(module graph-cycles

 (graph-cycles-fold)
		   
 (import scheme chicken data-structures )

 (require-extension srfi-1 iset matchable graph-scc )

(define (graph-cycles:error x . rest)
  (let ((port (open-output-string)))
    (let loop ((objs (cons x rest)))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'graph-cycles (get-output-string port)))
	  (begin (display (car objs) port)
		 (display " " port)
		 (loop (cdr objs)))))))

(define (graph-cycles-fold g f x)
  (define n ((g 'capacity)))
  
  (define in-scc (make-vector n (list -1 0)))
  (define cycles (make-bit-vector n))

  (define (traverse-scc scc x)
    (define witness (car scc))

    (define (order lst i)
      (match lst
	     (()  (void))
	     ((u . us)  (begin
			  (vector-set! in-scc u (list witness i))
			  (order us (fx+ 1 i))))
	     (else (graph-cycles:error 'traverse-scc ": invalid node list " lst))))
    

    (define (dfs n root u cycle x)
      (dfs-succ n root ((g 'in-edges) u) cycle x))

    (define (dfs-succ n root elst cycle x)
      (match elst 
	     (()  x)
	     (((v u . _) . es)   
	      (if (fx= root v) 
		  (dfs-succ n root es cycle (f (cons (car elst) cycle) x))
		  (let-values (((w m) (split-at (vector-ref in-scc v) 1)))
	            (if (or (not (fx= (car w) witness)) (fx<= (car m) n)
			    (bit-vector-ref cycles v))
			(dfs-succ n root es cycle x)
			(begin
			  (bit-vector-set! cycles v #t)
			  (let ((x (dfs n root v (cons (car elst) cycle) x)))
			    (bit-vector-set! cycles v #f)
			    (dfs-succ n root es cycle x)))))))
	     (else (graph-cycles:error 'traverse-scc ": invalid edge list " elst))))

    (define (has-back-edge elst n)
      (match elst 
	     (()  #f)
	     (((v . _) . es)   
		  (let-values (((w m) (split-at (vector-ref in-scc v) 1)))
                    (or (and (fx= (car w) witness) (fx>= (car m) n))
			(has-back-edge es n))))
	     (else (graph-cycles:error 'traverse-scc ": invalid edge list " elst))))

    (define (enum n us x)
      (match us
	     (()  x)
	     ((u . us)  (let ((x (if (has-back-edge ((g 'in-edges) u) n)
				     (dfs n u u (list) x) x)))
			  (enum (fx+ 1 n) us x)))
	     (else (graph-cycles:error 'traverse-scc ": invalid node list " us))))

    (begin (order scc 0)
	   (enum 0 scc x)))

  (graph-scc-fold g traverse-scc x))


)