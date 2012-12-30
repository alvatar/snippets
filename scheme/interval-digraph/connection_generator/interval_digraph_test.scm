

(require-library srfi-1 interval-digraph cis)
(import srfi-1 
	(prefix cis   cis:)
	(prefix interval-digraph   interval-digraph:)
	)

(define (interval-digraph-test)
  (let* ((N 100) (k 2)
	 
	 (g (interval-digraph:make-digraph  'depgraph  "interval  graph"))
	 
	 ;; add the nodes to the graph
	 (g ((g 'add-node-interval) (cis:interval 0 N)))

	 (g ((g 'add-node-interval) (cis:interval (* k N) (* (+ 1 k) N))))

	 )
    (let* ((sources  (cis:interval 0 N))
	   (targets  (cis:interval (* k N) (* (+ 1 k) N)))
	   (g        (cis:fold-left
		      (lambda (i g)
			((g 'add-edge-interval) (list i targets)))
		      g sources)
		     ))
      
      g)))

