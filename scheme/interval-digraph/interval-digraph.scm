;;
;; 
;; Persistent directed graph based on adjacency intervals.
;;
;; Copyright 2010-2012 Ivan Raikov and the Okinawa Institute of Science and Technology.
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

(module interval-digraph


 (make-digraph digraph-union digraph-disjoint-union digraph-rename
  make-random-gnp-digraph)

		   
 (import scheme chicken)
 (require-extension rb-tree)
 (require-library cis )
 (import (prefix cis cis:))
 (import (only data-structures alist-ref compose)
	 (only srfi-1 append-reverse delete-duplicates))

(define (interval-digraph:error x . rest)
  (let ((port (open-output-string)))
    (let loop ((objs (cons x rest)))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'digraph (get-output-string port)))
	  (begin (display (car objs) port)
		 (display " " port)
		 (loop (cdr objs)))))))

(define (alist-update k v alst #!optional (eq eq?))
  (cons (cons k v)
        (let recur ((alst alst) (ax '()))
          (cond ((null? alst) (reverse ax))
                ((eq k (car (car alst))) (recur (cdr alst) ax))
                (else (recur (cdr alst) (cons (car alst) ax)))))))

(define (node-set-lookup-compare x y)
  (if (cis:subset? x y)  0
      (let ((xmax (cis:get-max x))
	    (xmin (cis:get-min x))
	    (ymax (cis:get-max y))
	    (ymin (cis:get-min y)))
	(cond ((fx= ymax xmax) (fx- ymin xmin))
	      (else (fx- ymax xmax))))
      ))

	
(define (node-set-insdel-compare x y)
  (let ((xmax (cis:get-max x))
	(xmin (cis:get-min x))
	(ymax (cis:get-max y))
	(ymin (cis:get-min y)))
    (cond ((fx= ymax xmax) (fx- ymin xmin))
	  (else (fx- ymax xmax)))))


(define (make-prop-tree)
  (make-persistent-map node-set-lookup-compare 
		       insdel-key-compare: node-set-insdel-compare))

(define-record-type interval-digraph
  (make-interval-digraph name label nodes succs node-props edge-props )
  interval-digraph?
  (name        graph-name)
  (label       graph-label)
  (nodes       graph-nodes)
  (succs       graph-succs)
  (node-props  graph-node-props)
  (edge-props  graph-edge-props)
  )

(define (empty-graph name label)
  (make-interval-digraph name label cis:empty 
			 (make-persistent-map fx-) 
			 `((label . ,(make-prop-tree))) `((label . ,(make-prop-tree)))
			 ))

(define (interval-digraph-operations graph-instance)

  (define name        (graph-name graph-instance))
  (define label       (graph-label graph-instance))
  (define nodes       (graph-nodes graph-instance))
  (define succs       (graph-succs graph-instance))
  (define node-props  (graph-node-props graph-instance))
  (define edge-props  (graph-edge-props graph-instance))

  (define (get-nodes) (cis:elements nodes))
  
  (define (get-nodes-with-labels) 
    (let ((node-labels (alist-ref 'label node-props)))
      (cis:fold-right (lambda (i ax) 
			(let ((label ((node-labels 'get-value) (cis:singleton i) #f)))
			  (cons (list i label)  ax))) 
		      '() 
		      nodes)))


  (define (get-edges) 
    ((succs 'foldi)
     (lambda (i succ-set ax) 
       (cis:fold-left (lambda (j ax) (cons (list i j) ax)) 
		      ax succ-set))
     '()))


  (define (get-edges-with-labels) 
    ((succs 'foldi)
     (lambda (i succ-set ax) 
       (let* ((edge-labels (alist-ref 'label edge-props))
	      (i-labels    ((edge-labels 'get-value) (cis:singleton i) make-prop-tree)))
	 (cis:fold-left (lambda (j ax) 
			  (let ((l ((i-labels 'get-value) (cis:singleton j) #f)))
			    (cons (list i j l) ax))) 
			ax succ-set)))
     '() ))


  (define (order)  (cis:cardinal nodes) )

  (define (size)    ((succs 'fold) (lambda (succ-set ax) (+ (cis:cardinal succ-set) ax)) 0))

  (define capacity order)

  (define (add-node i #!key (label #f) )
    (make-interval-digraph 
     name label
     (cis:add i nodes)
     succs 
     (if label 
	 (let ((node-labels (alist-ref 'label node-props)))
	   (alist-update 'label ((node-labels 'put) (cis:singleton i) label) node-props))
	 node-props)
     edge-props
     ))

  (define (add-node-interval i #!key (label #f) )
    (make-interval-digraph 
     name label
     (cis:union i nodes)
     succs 
     (if label 
	 (let ((node-labels (alist-ref 'label node-props)))
	   (alist-update 'label ((node-labels 'put) i label) node-props))
	 node-props)
     edge-props
     ))
	    

  (define (add-edge e #!key (label #f))
    (cond ((and (pair? e) (pair? (cdr e)))

	   (let ((i (car e)) (j (cadr e)))

	     (and (cis:in? i nodes) (cis:in? j nodes) 

		    (let ((oi ((succs 'get-value) i cis:empty)))
		      
		      (let ((succs1 ((succs 'put) i (cis:add j oi))))
			
			(make-interval-digraph 
			 name label 
			 nodes 
			 succs1 
			 node-props
			 (if label 
			     (let* ((edge-labels  (alist-ref 'label edge-props))
				    (i-labels     ((edge-labels 'get-value) (cis:singleton i) make-prop-tree))
				    (i-labels1    ((i-labels 'put) (cis:singleton j) label))
				    (edge-labels1 ((edge-labels 'put) (cis:singleton i) i-labels1)))
			       (alist-update 'label edge-labels1  edge-props))
			     edge-props)
			 ))))
	     ))
	  (else (interval-digraph:error 'add-edge ": invalid edge " e))))


  (define (add-edge-interval e )
    (cond ((and (pair? e) (pair? (cdr e)))

	   (let ((i (car e)) (js (cadr e)))

	     (and (cis:in? i nodes) (cis:subset? js nodes) 

		    (let* ((oi ((succs 'get-value) i cis:empty)))
		      
		      (let ((succs1 ((succs 'put) i (cis:union js oi))))
 			
			(make-interval-digraph 
			 name label 
			 nodes 
			 succs1 
			 node-props
			 edge-props
			 ))))
	     ))
	  (else (interval-digraph:error 'add-edge-interval ": invalid edge interval " e))))

  

  (define (get-succ i)           (and (cis:in? i nodes) (cis:elements ((succs 'get-value) i cis:empty))))

  (define (get-succ-interval i)  (and (cis:in? i nodes) ((succs 'get-value) i cis:empty)))

  (define (out-edges i)          (and (cis:in? i nodes) 
				      (let ((succ-set ((succs 'get-value) i cis:empty)))
					(cis:fold-left (lambda (j ax) (cons (list i j) ax)) '() 
						       succ-set))))

  (define (has-edge i j)         (and (cis:in? i nodes)
				      (cis:in? j ((succs 'get-value) i cis:empty))))
				       
  
  (define (has-node i)           (cis:in? i nodes) )


  (define (has-node-interval i)  (cis:subset? i nodes) )


  (define (edge-property-list-keys)   (map car edge-props))

  (define (edge-property-list-map i)  (and (cis:in? i nodes)
					   (map 
					    (lambda (p) 
					      (let ((prop-key (car p))
						    (prop-map (cdr p)))
						(let ((i-prop ((prop-map 'get-value) (cis:singleton i) #f)))
						  (cons prop-key i-prop))))
					    edge-props)))

  (define (edge-property p i j)  (and (cis:in? i nodes) (cis:in? j nodes)  
				      (let* ((prop (alist-ref p edge-props))
					     (i-prop (and prop ((prop 'get-value) (cis:singleton i) #f))))
					 (and i-prop ((i-prop 'get-value) (cis:singleton j) #f)))))
    

  (define (edge-property-set p i j v)   (and (has-edge i j)
					     (make-interval-digraph name label
								    nodes succs 
								    node-props
								    (let* ((prop    (or (alist-ref p edge-props) (make-prop-tree)))
									   (i-prop  ((prop 'get-value) (cis:singleton i) make-prop-tree))
									   (i-prop1 ((i-prop 'put) (cis:singleton j) v))
									   (prop1   ((prop 'put) (cis:singleton i) i-prop1)))
								      (alist-update p prop1 edge-props))
								    )))

  (define (edge-interval-property p i j)   (and (cis:cis? i) (cis:cis? j) 
						(and (cis:subset? i nodes)  (cis:subset? j nodes)
						     (let* ((prop (alist-ref p edge-props))
							    (i-prop (and prop ((prop 'get-value) i #f))))
						       (and i-prop ((i-prop 'get-value) j #f))))))

  (define (edge-interval-property-set p i j v)   (and (cis:cis? i) (cis:cis? j) 
						      (and (cis:subset? i nodes) (cis:subset? j nodes) 
							   (make-interval-digraph name label
										  nodes succs 
										  node-props
										  (let* ((prop    (or (alist-ref p edge-props) (make-prop-tree)))
											 (i-prop  ((prop 'get-value) i make-prop-tree))
											 (i-prop1 ((i-prop 'put) j v))
											 (prop1   ((prop 'put) i i-prop1)))
										    
										    (alist-update p prop1 edge-props))
										  ))))

  (define (edge-interval-prototype p i j)   (and (cis:cis? i) (cis:cis? j) 
						(and (cis:subset? i nodes)  (cis:subset? j nodes)
						     (let* ((prop (alist-ref p edge-props))
							    (i-prop (and prop ((prop 'get-value) i #f))))
						       (let ((proto-function (and i-prop ((i-prop 'get-value) j #f))))
							 (and proto-function (proto-function graph-instance i j)))
						       ))
						))

  (define (edge-interval-prototype-set p i j v)   (and (cis:cis? i) (cis:cis? j) (procedure? v)
						      (and (cis:subset? i nodes) (cis:subset? j nodes) 
							   (make-interval-digraph name label
										  nodes succs 
										  node-props
										  (let* ((prop    (or (alist-ref p edge-props) (make-prop-tree)))
											 (i-prop  ((prop 'get-value) i make-prop-tree))
											 (i-prop1 ((i-prop 'put) j v))
											 (prop1   ((prop 'put) i i-prop1)))
										    (alist-update p prop1 edge-props))
										  ))
						      ))

  (define (node-property-list-keys)    (map car node-props))

  (define (node-property p i)    (and (cis:in? i nodes)
				      (let ((prop (alist-ref p node-props)))
					(and prop ((prop 'get-value) (cis:singleton i) #f)))))

				      
  (define (node-property-set p i v)    (and (cis:in?  i nodes) 
					    (make-interval-digraph name label
								   nodes succs 
								   (let* ((prop (or (alist-ref p node-props) (make-prop-tree)))
									  (prop1 ((prop 'put) (cis:singleton i) v)))
								     (alist-update p prop1 node-props))
								   edge-props
								   )))

  (define (node-interval-property p i)   (and (cis:cis? i) 
					      (and (cis:subset? i nodes)
						   (let ((prop  (alist-ref p node-props)))
						     (and prop ((prop 'get-value) i #f))))))


  (define (node-interval-property-set p i v)   (and (cis:cis? i)
						    (and (cis:subset? i nodes) 
							 (make-interval-digraph name label
										nodes
										succs 
										(let* ((prop  (or (alist-ref p node-props) (make-prop-tree)))
										       (prop1 ((prop 'put) i v)))
										  (alist-update p prop1 node-props))
										edge-props
										))))

  (define (node-label i)     (node-property 'label i))

  (define (node-label-set i v)   (node-property-set 'label i v))

  (define (foreach-node f)   (cis:foreach (lambda (i) (f i)) nodes))


  (define (foreach-node-with-label f #!key (default-label 'undefined))
    (let ((node-labels (alist-ref 'label node-props)))
      (cis:foreach (lambda (i) 
		     (let ((v ((node-labels 'get-value) (cis:singleton i) default-label)))
		       (f i v)))
		   nodes)))


  (define (foreach-edge f) 
    ((succs 'for-each-ascending) 
     (lambda (e) 
       (let ((i (car e)))
	 (cis:foreach (lambda (j) (f i j)) (cdr e)))
       )))


  (define (foreach-edge-with-property f p) 
    (let* ((props (alist-ref p edge-props))
	   (get-prop-value (and props (props 'get-value))) )
      ((succs 'for-each-ascending) 
       (lambda (e)  
	 (let* ((i (car e))
		(i-prop (and get-prop-value (get-prop-value (cis:singleton i) #f))))
	   (if i-prop
	       (cis:foreach (lambda (j) (f i j ((i-prop 'get-value) (cis:singleton j) #f))) (cdr e))
	       (cis:foreach (lambda (j) (f i j #f)) (cdr e))))
	   ))
       ))


  ;; Dispatcher
  (lambda (selector)
      (case selector
	;; accessors
	((name)                        name)
	((label)                       label)
	((nodes)                       get-nodes)
	((nodes-with-labels)           get-nodes-with-labels)
	((node-intervals)              nodes)
	((edges)                       get-edges)
	((edges-with-labels)           get-edges-with-labels)
	((order)                       order)
	((size)                        size)
	((capacity)                    capacity)
	((out-edges)                   out-edges)
	((succ)                        get-succ)
	((succ-interval)               get-succ-interval)
	((has-edge)                    has-edge)
	((has-node)                    has-node)
	((has-node-interval)           has-node-interval)
	((node-property-list-keys)          node-property-list-keys)
	((node-property)               node-property)
	((node-label)                  node-label)
	((node-interval-property)      node-interval-property)
	((edge-property-list-keys)         edge-property-list-keys)
	((edge-property-list-map)          edge-property-list-map)
	((edge-property)               edge-property)
	((edge-interval-property)      edge-interval-property)
	((edge-interval-prototype)     edge-interval-prototype)
	((foreach-node)                foreach-node)
	((foreach-node-with-label)     foreach-node-with-label)
        ((foreach-edge)                foreach-edge)        
	((foreach-edge-with-property)  foreach-edge-with-property)
	

	;; transformers
	((add-node)                    (compose interval-digraph-operations add-node))
	((add-node-interval)           (compose interval-digraph-operations add-node-interval))
	((add-edge)                    (compose interval-digraph-operations add-edge))
	((add-edge-interval)           (compose interval-digraph-operations add-edge-interval))
	((node-label-set)              (compose interval-digraph-operations node-label-set))
	((node-property-set)           (compose interval-digraph-operations node-property-set))
	((node-interval-property-set)  (compose interval-digraph-operations node-interval-property-set))
	((edge-property-set)           (compose interval-digraph-operations edge-property-set))
	((edge-interval-property-set)  (compose interval-digraph-operations edge-interval-property-set))
	((edge-interval-prototype-set) (compose interval-digraph-operations edge-interval-prototype-set))
	
        (else
          (interval-digraph:error 'selector ": unknown message " selector " sent to a graph"))))
)




(define (make-digraph name label) (interval-digraph-operations (empty-graph name label)))


(define (merge a b compare merge-fn)
  (let recur ((a a) (b b) (l '()))
    (cond ((and (null? a) (null? b)) (reverse l))
	  ((null? a) (append-reverse l b))
	  ((null? b) (append-reverse l a))
	  (else
	   (let ((c (compare (car a) (car b))))
	     (cond ((negative? c)  (recur (cdr a) b (cons (car a) l)))
		   ((zero? c)      (recur (cdr a) (cdr b) (cons (merge-fn (car a) (car b)) l)))
		   ((positive? c)  (recur a (cdr b) (cons (car b) l))))))
	  )))



(define (digraph-union a b merge-label)

  (define (merge-nodes a b)
    (merge a b
     (lambda (x y) (fx- (car x) (car y)))
     (lambda (x y) x)))

  (define (merge-nodes-with-labels a b)
    (merge a b
     (lambda (x y) (fx- (car x) (car y)))
     (lambda (x y) (list (car x) (merge-label (cadr x) (cadr y))))))

  (define (merge-edges a b)
    (merge a b
     (lambda (x y) (let ((c (fx- (car x) (car y))))
		     (if (zero? c) (fx- (cadr x) (cadr y)) c)))
     (lambda (x y) x)))

  (define (merge-edges-with-labels a b)
    (merge a b
	   (lambda (x y) (let ((c (fx- (car x) (car y))))
			   (if (zero? c) (fx- (cadr x) (cadr y)) c)))
	   (lambda (x y) (list (car x) (cadr x) (merge-label (caddr x) (caddr y))))))

  
  (let recur ((a a) (b b))

    (let* (;; accessors
	   (name                  (string-append "union " (a 'name) (b 'name)))
	   (label                 (merge-label (a 'label) (b 'label)))
	   (nodes                 (lambda () (cis:elements (cis:union (a 'node-intervals) (b 'node-intervals)))))
	   (nodes-with-labels     (lambda ()
				    (merge-nodes-with-labels
				     (a 'nodes-with-labels)
				     (b 'nodes-with-labels))))
	   (node-intervals        (lambda () (cis:union (a 'node-intervals) (b 'node-intervals))))
	   (edges                 (lambda () (merge-edges (a 'edges) (b 'edges))))
	   (edges-with-labels     (lambda () (merge-edges-with-labels 
					      (a 'edges-with-labels) 
					      (b 'edges-with-labels))))
	   (order                  (lambda () (cis:cardinal (cis:union (a 'node-intervals) (b 'node-intervals)))))
	   (size                   (lambda () (length (edges))))
	   (capacity               order)
	   (out-edges                (lambda (i)   (merge-edges ((a 'out-edges) i) ((b 'out-edges) i))))
	   (succ                     (lambda (i)   (cis:elements (cis:union ((a 'succ-interval) i)  ((b 'succ-interval) i)))))
	   (succ-interval            (lambda (i)   (cis:union ((a 'succ-interval) i)  ((b 'succ-interval) i))))
	   (has-edge                 (lambda (i j) (or ((a 'has-edge) i j) ((b 'has-edge) i j))))
	   (has-node                 (lambda (i)   (or ((a 'has-node) i) ((b 'has-node) i))))
	   (has-node-interval        (lambda (i)   (or ((a 'has-node-interval) i) ((b 'has-node-interval) i))))
	   (node-property-list-keys       (lambda ()   (delete-duplicates (append ((a 'node-property-list-keys)) ((b 'node-property-list-keys))))))
	   (node-property            (lambda (p i)  (or ((a 'node-property) p i) ((b 'node-property) p i))))
	   (node-interval-property   (lambda (p i)  (or ((a 'node-interval-property) p i) ((b 'node-interval-property) p i))))
	   (node-label               (lambda (i)    (or ((a 'node-label) i) ((b 'node-label) i))))
	   (edge-property-list-keys  (lambda () (delete-duplicates (append ((a 'edge-property-list-keys)) ((b 'edge-property-list-keys))))))
	   (edge-property-list-map   (lambda () (delete-duplicates (append ((a 'edge-property-list-map)) ((b 'edge-property-list-map))))))
	   (edge-property            (lambda (p i j)  (or ((a 'edge-property) p i j) ((b 'edge-property) i j))))
	   (edge-interval-property   (lambda (p i j)  (or ((a 'edge-interval-property) p i j) ((b 'edge-interval-property) i j))))
	   (foreach-node             (lambda (f) (for-each f (nodes))))
	   (foreach-node-with-label  (lambda (f) (for-each f (nodes-with-labels))))
	   (foreach-edge             (lambda (f) (for-each f (edges))))

	   ;; transformers
	   (add-node
	    (lambda (n #!key (label #f))
	      (recur ((a 'add-node) n label: label) ((b 'add-node) n label: label))))
	   (add-node-interval
	    (lambda (i #!key (label #f))
	      (recur ((a 'add-node-interval) i label: label) ((b 'add-node-interval) i label: label))))
	   (add-edge-interval  
	    (lambda (e)
	      (recur ((a 'add-edge-interval) e) ((b 'add-edge-interval) e))))
			  
	   (edge-interval-property-set      
	    (lambda (p i j v)  (let* ((a1 ((a 'edge-interval-property-set) p i j v))
				      (b1 (and (not a1) ((b 'edge-interval-property-set) p i j v))))
				 (cond (a1 (recur a1 b))
				       (b1 (recur a b1))
				       (else (recur a b))))))
	   (node-interval-property-set      
	    (lambda (p i v)    (let* ((a1 ((a 'node-interval-property-set) p i v))
				      (b1 (and (not a1) ((b 'node-interval-property-set) p i v))))
				 (cond (a1 (recur a1 b))
				       (b1 (recur a b1))
				       (else (recur a b))))))
	   (node-label-set         
	    (lambda (i v)      (let* ((a1 ((a 'node-label-set) i v))
				      (b1 (and (not a1) ((b 'node-label-set) i v))))
				 (cond (a1 (recur a1 b))
				       (b1 (recur a b1))
				       (else (recur a b))))))
	   (node-property-set      
	    (lambda (p i v)    (let* ((a1 ((a 'node-property-set) p i v))
				      (b1 (and (not a1) ((b 'node-property-set) p i v))))
				 (cond (a1 (recur a1 b))
				       (b1 (recur a b1))
				       (else (recur a b))))))
	   (edge-property-set      
	    (lambda (p i j v)  (let* ((a1 ((a 'edge-property-set) p i j v))
				      (b1 (and (not a1) ((b 'edge-property-set) p i j v))))
				 (cond (a1 (recur a1 b))
				       (b1 (recur a b1))
				       (else (recur a b))))))
				 
	   )

    (lambda (selector)
      (case selector
	;; accessors
	((name)                        name)
	((label)                       label)
	((nodes)                       nodes)
	((nodes-with-labels)           nodes-with-labels)
	((node-intervals)              node-intervals)
	((edges)                       edges)
	((edges-with-labels)           edges-with-labels)
	((order)                       order)
	((size)                        size)
	((capacity)                    capacity)
	((out-edges)                   out-edges)
	((succ)                        succ)
	((succ-interval)               succ-interval)
	((has-edge)                    has-edge)
	((has-node)                    has-node)
	((has-node-interval)           has-node-interval)
	((node-property-list-keys)          node-property-list-keys)
	((node-property)               node-property)
	((node-interval-property)      node-interval-property)
	((node-label)                  node-label)
	((edge-property)               edge-property)
	((edge-property-list-keys)     edge-property-list-keys)
	((edge-property-list-map)      edge-property-list-map)
	((foreach-node)                foreach-node)
	((foreach-node-with-label)     foreach-node-with-label)
        ((foreach-edge)                foreach-edge)

	;; transformers
	((add-node)                    add-node)
	((add-node-interval)           add-node-interval)
	((add-edge-interval)           add-edge-interval)
	((node-label-set)              node-label-set)
	((node-property-set)           node-property-set)
	((node-interval-property-set)  node-interval-property-set)
	((edge-property-set)           edge-property-set)
	((edge-interval-property-set)  edge-interval-property-set)
	
        (else
          (interval-digraph:error 'selector ": unknown message " selector " sent to a graph"))))
    
    )))
			 
;;
;; Adds a number k to all node ids of the graph
;;


(define (digraph-rename k a)

  (define (rename-nodes ns) (map (lambda (x) (list (fx+ k x))) ns))
  (define (rename-nodes-with-labels ns) (map (lambda (x) (list (fx+ k (car x)) (cadr x))) ns))
  (define (rename-edges es) (map (lambda (e) (list (fx+ k (car e)) (fx+ k (cadr e)))) es))
  (define (rename-edges-with-labels es) (map (lambda (e) (list (fx+ k (car e)) (fx+ k (cadr e)) (caddr e))) es))

  (let recur ((a a))

    (let* (;; accessors
	   (name                     (a 'name))
	   (label                    (a 'label))
	   (nodes                    (lambda () (cis:elements (cis:shift k (a 'node-intervals) ))))
	   (nodes-with-labels        (lambda () (rename-nodes-with-labels (a 'nodes-with-labels))))
	   (node-intervals           (lambda () (cis:shift k (a 'node-intervals) )))
	   (edges                    (lambda () (rename-edges (a 'edges) )))
	   (edges-with-labels        (lambda () (rename-edges-with-labels (a 'edges-with-labels) )))
	   (order                    (lambda () (a 'order)))
	   (size                     (a 'size))
	   (capacity                 order)
	   (out-edges                (lambda (i)      (rename-edges ((a 'out-edges) (fx- i k)))))
	   (succ                     (lambda (i)      (cis:elements (cis:shift k ((a 'succ-interval) (fx- i k) )))))
	   (succ-interval            (lambda (i)      (cis:shift k ((a 'succ-interval) (fx- i k) ))))
	   (has-edge                 (lambda (i j)    ((a 'has-edge) (fx- i k) (fx- j k))))
	   (has-node                 (lambda (i)      ((a 'has-node) (fx- i k))))
	   (has-node-interval        (lambda (i)      ((a 'has-node-interval) (cis:shift (fxneg k) i))))
	   (node-property-list-keys       (a 'node-property-list-keys))
	   (node-property            (lambda (p i)    ((a 'node-property) p (fx- i k) )))
	   (node-interval-property   (lambda (p i)    ((a 'node-interval-property) p (cis:shift (fxneg k) i) )))
	   (node-label               (lambda (i)      ((a 'node-label) (fx- i k))))
	   (edge-property-list-keys       (a 'edge-property-list-keys))
	   (edge-property-list-map        (a 'edge-property-list-map))
	   (edge-property            (lambda (p i j)  ((a 'edge-property) p (fx- i k) (fx- j k) )))
	   (edge-interval-property   (lambda (p i j)  ((a 'edge-interval-property) p (cis:shift (fxneg k) i) (cis:shift (fxneg k) j) )))
	   (foreach-node             (lambda (f)      (for-each (lambda (i) (f (fx+ i k))) (nodes))))
	   (foreach-node-with-label  (lambda (f)      (for-each (lambda (x) (f (fx+ (car x) k) (cadr x))) (nodes-with-labels))))
	   (foreach-edge             (lambda (f)      (for-each (lambda (e) (f (list (fx+ (car e) k) (fx+ (cadr e) k)))) (edges))))
	   
	   ;; transformers
	   (add-node    (lambda (n #!key (label #f))  (recur ((a 'add-node) (fx- n k) label: label) )))

	   (add-node-interval    (lambda (i #!key (label #f))  (recur ((a 'add-node-interval) (cis:shift (fxneg k) i) label: label) )))

	   (add-edge    (lambda (e #!key (label #f))  (recur ((a 'add-edge) (list (fx- (car e) k) (fx- (cadr e) k)) label: label) )))
	   (add-edge-interval (lambda (e)  (recur ((a 'add-edge-interval) (list (fx- (car e) k) (fx- (cadr e) k)) ))))

	   (edge-interval-property-set      
	    (lambda (p i j v)  (recur ((a 'edge-interval-property-set) p 
				       (cis:shift (fxneg k) i) (cis:shift (fxneg k) j) v))))

	   (node-interval-property-set      
	    (lambda (p i v)    (recur ((a 'node-interval-property-set) p 
				       (cis:shift (fxneg k) i) v))))

	   (node-label-set         
	    (lambda (i v)      (recur ((a 'node-label-set) (fx- i k) v))))

	   (node-property-set      
	    (lambda (p i v)    (recur ((a 'node-property-set) p (fx- i k) v))))

	   (edge-property-set      
	    (lambda (p i j v)  (recur ((a 'edge-property-set) p (fx- i k) (fx- j k) v))))

	   )
      
    (lambda (selector)
      (case selector
	;; accessors
	((name)                        name)
	((label)                       label)
	((nodes)                       nodes)
	((nodes-with-labels)           nodes-with-labels)
	((node-intervals)              node-intervals)
	((edges)                       edges)
	((edges-with-labels)           edges-with-labels)
	((order)                       order)
	((size)                        size)
	((capacity)                    capacity)
	((out-edges)                   out-edges)
	((succ)                        succ)
	((succ-interval)               succ-interval)
	((has-edge)                    has-edge)
	((has-node)                    has-node)
	((has-node-interval)           has-node-interval)
	((node-property-list-keys)          node-property-list-keys)
	((node-property)               node-property)
	((node-interval-property)      node-interval-property)
	((node-label)                  node-label)
	((edge-property-list-keys)     edge-property-list-keys)
	((edge-property-list-map)      edge-property-list-map)
	((edge-property)               edge-property)
	((foreach-node)                foreach-node)
	((foreach-node-with-label)     foreach-node-with-label)
        ((foreach-edge)                foreach-edge)

	;; transformers
	((add-node)                    add-node)
	((add-node-interval)           add-node-interval)
	((add-edge)                    add-edge)
	((add-edge-interval)           add-edge-interval)
	((node-label-set)              node-label-set)
	((node-property-set)           node-property-set)
	((node-interval-property-set)  node-interval-property-set)
	((edge-property-set)           edge-property-set)
	((edge-interval-property-set)  edge-interval-property-set)
	
        (else
          (interval-digraph:error 'selector ": unknown message " selector " sent to a graph"))))
    
    )))


(define (digraph-disjoint-union a b) (digraph-union a (digraph-rename ((a 'capacity)) b)))

;;
;; Naive implementation: randomly choosing edges from NxN possibilities with probability P 
;;

(define (make-random-gnp-digraph name label N P R S loops)

  (if (< N 10) (error 'make-random-gnp-digraph "N is too small" N))
  (if (not (and (< 0 P) (<= P 1))) (error 'make-random-gnp-digraph "P must be in the interval (0, 1]"))

  (let* ((E     (* N N))
	 (nodes (cis:interval 1 N))
	 (a     (make-digraph name label))
	 (a     ((a 'add-node-interval) nodes)))
    
    (let recur ((a a) (s S) (e 0))

      (if (> e E) a

	  (let* ((i (inexact->exact (R N P s)))
		 (j (inexact->exact (R N P s))))

	    (if (or (zero? i) (zero? j) (and (= i j) (not loops)) ((a 'has-edge) i j))
		(recur a s (+ 1 e))
		(recur ((a 'add-edge) (list i j)) s (+ 1 e))
		)))
    )))


)
