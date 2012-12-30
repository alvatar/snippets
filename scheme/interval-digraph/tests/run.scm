;;
;;
;; Verifying the digraph package. Code adapted from the Boost graph
;; library dependency example.
;;
;; Copyright 2007-2010 Ivan Raikov and the Okinawa Institute of Science and Technology
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
;;
;;

(require-library srfi-1 test interval-digraph cis random-mtzig)
(import srfi-1 test
	(prefix cis   cis:)
	(prefix interval-digraph   interval-digraph:)
	(only random-mtzig random-mtzig:init random-mtzig:randb!)
	)


(define used-by
  (list
    (cons 'dax_h 'foo_cpp) (cons 'dax_h 'bar_cpp) (cons 'dax_h 'yow_h)
    (cons 'yow_h 'bar_cpp) (cons 'yow_h 'zag_cpp) (cons 'boz_h 'bar_cpp)
    (cons 'boz_h 'zig_cpp) (cons 'boz_h 'zag_cpp) (cons 'zow_h 'foo_cpp)
    (cons 'foo_cpp 'foo_o) (cons 'foo_o 'libfoobar_a)
    (cons 'bar_cpp 'bar_o) (cons 'bar_o 'libfoobar_a)
    (cons 'libfoobar_a 'libzigzag_a) (cons 'zig_cpp 'zig_o)
    (cons 'zig_o 'libzigzag_a) (cons 'zag_cpp 'zag_o)
    (cons 'zag_o 'libzigzag_a) (cons 'libzigzag_a 'killerapp)))


(define node-list
  (delete-duplicates
   (concatenate (list (map car used-by) (map cdr used-by)))))


(define node-ids
  (list-tabulate (length node-list) values))

(define node-map  (zip node-list node-ids)) 

(test-group "basic digraph test"
  (let* ((g (interval-digraph:make-digraph  'depgraph  "dependency graph"))
	 
	 ;; add the nodes to the graph
	 (g (fold (lambda (i n g) ((g 'add-node) i label: n))
		  g node-ids node-list)))

    ;; make sure all nodes got inserted
    (test "add nodes to the graph"
	  (map first
	       '((14 killerapp)
		 (13 libzigzag_a) (12 zag_o) (11 zag_cpp)
		 (10 zig_o) (9 zig_cpp) (8 libfoobar_a) (7 bar_o)
		 (6 bar_cpp) (5 foo_o) (4 foo_cpp) (3 zow_h) (2 boz_h)
		 (1 yow_h) (0 dax_h)))
	  ((g 'nodes)))

    (let ((g1  ;; add the edges to the graph
	   (fold (lambda (e g)
		   (let* ((ni (car e))
			  (nj (cdr e))
			  (i (car (alist-ref ni node-map)))
			  (j (car (alist-ref nj node-map))))
		     ((g 'add-edge) (list i j) label: (format "~A->~A" ni nj))))
		 g used-by)))

      ;; make sure all edges got correctly created
      (test "add edges to the graph"
	    '((13 14 "libzigzag_a->killerapp") (12 13 "zag_o->libzigzag_a")
	      (11 12 "zag_cpp->zag_o") (10 13 "zig_o->libzigzag_a")
	      (9 10 "zig_cpp->zig_o") (8 13 "libfoobar_a->libzigzag_a")
	      (7 8 "bar_o->libfoobar_a") (6 7 "bar_cpp->bar_o")
	      (5 8 "foo_o->libfoobar_a") (4 5 "foo_cpp->foo_o")
	      (3 4 "zow_h->foo_cpp") (2 6 "boz_h->bar_cpp")
	      (2 9 "boz_h->zig_cpp") (2 11 "boz_h->zag_cpp")
	      (1 6 "yow_h->bar_cpp") (1 11 "yow_h->zag_cpp") 
	      (0 1 "dax_h->yow_h") (0 4 "dax_h->foo_cpp") 
	      (0 6 "dax_h->bar_cpp") )
    	    ((g1 'edges-with-labels)))

	  )))


(test-group "interval digraph test"
  (let* ((N 100) (k 2)
	 
	 (g (interval-digraph:make-digraph  'depgraph  "interval  graph"))
	 
	 ;; add the nodes to the graph
	 (g ((g 'add-node-interval) (cis:interval 0 N)))

	 (g ((g 'add-node-interval) (cis:interval (* k N) (* (+ 1 k) N))))

	 )

    (let ((nset (list-tabulate (+ 1 N) identity)) (delta (* k N)))
      ;; make sure all nodes got inserted
      (test "add nodes to the graph"
	    (reverse (append  nset  (map (lambda (x) (+ x delta)) nset)))
	    ((g 'nodes))))

    (let* ((sources  (cis:interval 0 N))
	   (targets  (cis:interval (* k N) (* (+ 1 k) N)))
	   (g        (cis:fold-left
		      (lambda (i g)
			((g 'add-edge-interval) (list i targets)))
		      g sources)
		     ))
      
      (test "add edges to the graph" 
	    (cis:fold-right (lambda (x ax) (cis:fold-left (lambda (y ax) (cons (list x y) ax)) ax targets)) '() sources)
	    ((g 'edges)))

      (let ((g ((g 'edge-interval-property-set) 'test sources targets "chicken")))

	(test "edge-property" "chicken" 
	      ((g 'edge-property) 'test 10 220))

	(test "edge-property-list-map" "chicken"  
	      (((alist-ref 'test ((g 'edge-property-list-map) 10)) 'get-value) 
	       (cis:singleton 220)))

	((g 'foreach-edge)
	 (lambda (i j) 
	   (test "foreach-edge" #t (cis:in? i sources) )
	   (test "foreach-edge" #t (cis:in? j targets) )))
		 

	((g 'foreach-edge-with-property)
	 (lambda (i j v) 
	   (test "foreach-edge-with-property" #t (cis:in? i sources) )
	   (test "foreach-edge-with-property" #t (cis:in? j targets) )
	   (if (and (= i 10) (= j 220))
	       (test "foreach-edge-with-property" "chicken" v))
	   )
	 'test
	 )
		 

      ))
    ))

(test-group "random graph test"

  (let* ((g (interval-digraph:make-random-gnp-digraph  
	     'gnp-graph  "random G(N=100,P=0.2) graph"
	     100 0.2 random-mtzig:randb! (random-mtzig:init 48)
	     #f)))
	 
    (print ((g 'edges)))
    ))

(test-exit)
