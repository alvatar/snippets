;;
;; 
;; Breadth-first search in a graph
;; Based on code from MLRISC
;;
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
;; <http://www.gnu.org/licenses/>.
;;
;;

(module graph-bfs

 (graph-bfs-foreach
  graph-bfs-fold
  graph-bfs-dist)
		   
 (import scheme chicken data-structures )

 (require-extension srfi-1 srfi-4 iset matchable )


(define (graph-bfs:error x . rest)
  (let ((port (open-output-string)))
    (let loop ((objs (cons x rest)))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'graph-bfs (get-output-string port)))
	  (begin (display (car objs) port)
		 (display " " port)
		 (loop (cdr objs)))))))

(define (graph-bfs-foreach g fn fe roots)

  (define (traverse visited l r)
    (match l (()  (match r 
			 (()       visited)
			 ((x . _)  (traverse visited (reverse r) (list)))
			 (else (graph-bfs:error 'bfs-foreach ": invalid node list " r))))
	   ((n . l1)  (begin
			(fn n)
			(traverse-edges visited ((g 'out-edges) n) l1 r)))
	   (else (graph-bfs:error 'bfs-foreach ": invalid node list " l))))

  (define (traverse-edges visited elst l r)
    (match elst
	   (()  (traverse visited l r))
	   (((i j . _) . es)  (if (bit-vector-ref visited j)
				   (traverse-edges visited es l r)
				   (let ((visited (bit-vector-set! visited j #t)))
				     (fe (car elst))
				     (traverse-edges visited es l (cons j r)))))
	   (else (graph-bfs:error 'bfs-foreach ": invalid edge list " elst))))

  (define (traverse-roots visited ns l r)
    (match ns
	   (()  (traverse visited l r))
	   ((n . ns1)  (if (bit-vector-ref visited n)
			   (traverse-roots visited ns1 l r)
			   (let ((visited (bit-vector-set! visited n #t)))
			     (fn n)
			     (traverse-roots visited ns1 l (cons n r)))))
	   (else (graph-bfs:error 'bfs-foreach ": invalid node list " ns))))
    
  
  (traverse-roots (make-bit-vector ((g 'capacity))) roots (list) (list))
  (void))


(define (graph-bfs-fold g fn fe roots x y)

  (define (traverse visited l r x y)
    (match l (()  (match r 
			 (()       (values x y))
			 ((_ . _)  (traverse visited (reverse r) (list) x y))
			 (else (graph-bfs:error 'bfs-foreach ": invalid node list " r))))
	   ((n . l1)  (let ((x1 (fn n x)))
			(traverse-edges visited ((g 'out-edges) n) l1 r x1 y)))
	   (else (graph-bfs:error 'bfs-foreach ": invalid node list " l))))

  (define (traverse-edges visited elst l r x y)
    (match elst
	   (()  (traverse visited l r x y))
	   (((i j . _) . es)  (if (bit-vector-ref visited j)
				   (traverse-edges visited es l r x y)
				   (let ((visited (bit-vector-set! visited j #t))
					 (y1  (fe (car elst) y)))
				     (traverse-edges visited es l (cons j r) x y1))))
	   (else (graph-bfs:error 'bfs-foreach ": invalid edge list " elst))))

  (define (traverse-roots visited ns l r x y)
    (match ns
	   (()  (traverse visited l r x y))
	   ((n . ns1)  (if (bit-vector-ref visited n)
			   (traverse-roots visited ns1 l r x y)
			   (let ((visited (bit-vector-set! visited n #t)))
			     (traverse-roots visited ns1 l (cons n r) x y))))
	   (else (graph-bfs:error 'bfs-foreach ": invalid node list " ns))))
    
  
  (traverse-roots (make-bit-vector ((g 'capacity))) roots (list) (list) x y))


(define (graph-bfs-dist g roots)
  (define n        ((g 'capacity)))
  (define d        (make-s32vector n -1))
  (define dmax     0)
    
  (define (traverse l r)
    (match l (()  (match r 
			 (()       (void))
			 ((x . _)  (traverse (reverse r) (list)))
			 (else (graph-bfs:error 'bfs-dist ": invalid node list " r))))
	   ((n . l1)  (begin
			(traverse-edges ((g 'out-edges) n) l1 r)))
	   (else (graph-bfs:error 'bfs-dist ": invalid node list " l))))


  (define (traverse-edges elst l r)
    (match elst
	   (()  (traverse l r))
	   (((i j . _) . es)  (let ((di (s32vector-ref d i)))
				(set! dmax (max dmax (fx+ 1 di)))
				(s32vector-set! d j (fx+ 1 di))
				(traverse-edges es l (cons j r))))
	   (else (graph-bfs:error 'bfs-dist ": invalid edge list " elst))))

  (define (traverse-roots ns l r)
    (match ns
	   (()  (traverse l r))
	   ((n . ns1)  (if (fx>= (s32vector-ref d n) 0)
			   (traverse-roots ns1 l r)
			   (begin (s32vector-set! d n 0)
				  (traverse-roots ns1 l (cons n r)))))
	   (else (graph-bfs:error 'bfs-dist ": invalid node list " ns))))
    

  (traverse-roots roots (list) (list))
  (values d dmax))
)