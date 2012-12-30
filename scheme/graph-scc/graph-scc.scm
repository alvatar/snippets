;;
;;
;; Compute strongly connected components (SCC) of a graph, using
;; Tarjan's algorithm.
;; 
;; Based on code from MLRISC
;;
;; Copyright 2007-2011 Ivan Raikov and the Okinawa Institute of Science and Technology.
;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;


(module graph-scc

 (graph-scc-fold)
		   
 (import scheme chicken data-structures extras)

 (require-extension srfi-1 srfi-4 iset matchable )

(define (graph-scc:error x . rest)
  (let ((port (open-output-string)))
    (let loop ((objs (cons x rest)))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'graph-scc (get-output-string port)))
	  (begin (display (car objs) port)
		 (display " " port)
		 (loop (cdr objs)))))))

(define (scc-fold n nodes out-edges f initial)
  (define onq    (make-bit-vector n))
  (define dfsnum (make-s32vector n -1))

  (define (dfs v num q state)
    (define dfsnum-v num)

    (define (pop q scc state)
      (if (queue-empty? q) state
	  (let* ((x    (queue-remove! q))
		 (scc  (cons x scc)))
	    (bit-vector-set! onq x #f)
	    (if (fx= x v) (values q (f scc state))
		(pop q scc state)))))

    (define (fe elst num q low-v state)
      (match elst
	     (()  (values num q low-v state))
	     (((_ w _) . es) 
	      (let ((dfsnum-w   (s32vector-ref dfsnum w)))
		(if (fx>= dfsnum-w 0)
		    (if (and (fx< dfsnum-w dfsnum-v)
			     (bit-vector-ref onq w))
			(fe es num q (min dfsnum-w low-v) state)
			(fe es num q low-v state))
		    (let-values (((num q dfsnum-w low-w state) (dfs w num q state)))
				(fe es num q (min low-v low-w) state)))))
	     (else (graph-scc:error 'scc-fold ": invalid edge list " elst))))

    (begin
      (s32vector-set! dfsnum v dfsnum-v)
      (bit-vector-set! onq v #t)
      (queue-push-back! q v)
      (let-values (((num q low-v state)  
		    (fe (out-edges v) (fx+ 1 num) q dfsnum-v state)))
		  (let-values (((q state)  (if (fx= low-v dfsnum-v) 
					       (pop q (list) state)
					       (values q state))))
			      (values num q dfsnum-v low-v state)))))
			
  (define (dfs-all ns state)
    (match ns
	   (()  state)
	   ((n . ns1)  (if (fx>= (s32vector-ref dfsnum n) 0)
			   (dfs-all ns1 state)
			   (let-values (((d1 d2 d3 d4 state)  (dfs n 0 (make-queue) state)))
				       (dfs-all ns1 state))))
	   (else  (graph-scc:error 'dfs-all ": invalid node list " ns))))

  
  (dfs-all nodes initial))


(define (graph-scc-fold g f initial)
  (scc-fold ((g 'capacity)) (map car ((g 'nodes))) (g 'out-edges)
	    f initial))

)