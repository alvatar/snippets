;;
;;
;; Verifying the digraph package. Code adapted from the Boost graph
;; library dependency example.
;;
;; Copyright 2007-2012 Ivan Raikov and the Okinawa Institute of Science and Technology.
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


(require-library srfi-1 srfi-13 digraph graph-dfs test)
(import srfi-1 srfi-13 digraph graph-dfs test)


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


(test-group "dfs test"

  (let ((g (make-digraph 'depgraph "dependency graph")))

    ;; add the nodes to the graph
    (for-each (lambda (i n) ((g 'add-node!) i n))
	      node-ids node-list)
    
    ;; make sure all nodes got inserted
    (test "add nodes to the graph"
	  ((g 'nodes))
	  '((14 killerapp)
	    (13 libzigzag_a) (12 zag_o) (11 zag_cpp)
	    (10 zig_o) (9 zig_cpp) (8 libfoobar_a) (7 bar_o)
	    (6 bar_cpp) (5 foo_o) (4 foo_cpp) (3 zow_h) (2 boz_h)
	    (1 yow_h) (0 dax_h)))
  
    ;; add the edges to the graph
    (for-each (lambda (e)
		(let* ((ni (car e))
		       (nj (cdr e))
		       (i (car (alist-ref ni node-map)))
		       (j (car (alist-ref nj node-map))))
		  ((g 'add-edge!) (list i j (format "~A->~A" ni nj)))))
	      used-by)

  ;; make sure all edges got correctly created
  (test "added edges to the graph"
	((g 'edges))
	'((13 14 "libzigzag_a->killerapp") (12 13 "zag_o->libzigzag_a")
	  (11 12 "zag_cpp->zag_o") (10 13 "zig_o->libzigzag_a")
	  (9 10 "zig_cpp->zig_o") (8 13 "libfoobar_a->libzigzag_a")
	  (7 8 "bar_o->libfoobar_a") (6 7 "bar_cpp->bar_o")
	  (5 8 "foo_o->libfoobar_a") (4 5 "foo_cpp->foo_o")
	  (3 4 "zow_h->foo_cpp") (2 11 "boz_h->zag_cpp")
	  (2 9 "boz_h->zig_cpp") (2 6 "boz_h->bar_cpp")
	  (1 11 "yow_h->zag_cpp") (1 6 "yow_h->bar_cpp")
	  (0 1 "dax_h->yow_h") (0 6 "dax_h->bar_cpp") (0 4 "dax_h->foo_cpp")))

  (let ((roots  ((g 'roots)))
	(str    (open-output-string)))
	    
    (graph-dfs-foreach g 
		       (lambda (n) (fprintf str "node ~A; " n)) 
		       (lambda (e) (fprintf str "edge ~A; " e)) 
		       roots)

    (test "DFS iteration"
	  "node 3; edge (3 4 zow_h->foo_cpp); node 4; edge (4 5 foo_cpp->foo_o); node 5; edge (5 8 foo_o->libfoobar_a); node 8; edge (8 13 libfoobar_a->libzigzag_a); node 13; edge (13 14 libzigzag_a->killerapp); node 14; node 2; edge (2 11 boz_h->zag_cpp); node 11; edge (11 12 zag_cpp->zag_o); node 12; edge (2 9 boz_h->zig_cpp); node 9; edge (9 10 zig_cpp->zig_o); node 10; edge (2 6 boz_h->bar_cpp); node 6; edge (6 7 bar_cpp->bar_o); node 7; node 0; edge (0 1 dax_h->yow_h); node 1; "
	  (get-output-string str))
    
    
    (test "DFS fold"
     (graph-dfs-fold g 
		     (lambda (n ax) (cons (list 'node n) ax)) 
		     (lambda (e ax) (cons (list 'edge e) ax)) 
		     roots (list) (list))
     (values
      (reverse '((node 3) (node 4) (node 5) (node 8) (node 13) (node 14) 
		 (node 2) (node 11) (node 12) (node 9) (node 10) (node 6) 
		 (node 7) (node 0) (node 1)))
      (reverse '((edge (3 4 "zow_h->foo_cpp")) (edge (4 5 "foo_cpp->foo_o")) 
		 (edge (5 8 "foo_o->libfoobar_a")) 
		 (edge (8 13 "libfoobar_a->libzigzag_a")) 
		 (edge (13 14 "libzigzag_a->killerapp")) 
		 (edge (2 11 "boz_h->zag_cpp")) 
		 (edge (11 12 "zag_cpp->zag_o")) (edge (2 9 "boz_h->zig_cpp")) 
		 (edge (9 10 "zig_cpp->zig_o")) (edge (2 6 "boz_h->bar_cpp")) 
		 (edge (6 7 "bar_cpp->bar_o")) (edge (0 1 "dax_h->yow_h"))))))

    (test "DFS depth"
     (graph-dfs-depth g  (list 0))
     (s32vector 0 1 -1 -1 1 2 2 3 4 -1 -1 2 3 4 5))

    (test "DFS depth"
     (graph-dfs-depth g  roots)
     (s32vector 0 1 0 0 1 2 1 2 3 1 2 1 2 4 5))

    )
  ))

(test-exit)
