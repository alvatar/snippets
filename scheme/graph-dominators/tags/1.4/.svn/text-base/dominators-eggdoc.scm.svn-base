
(use eggdoc)

(define doc
  `((eggdoc:begin
     (name "graph-dominators")
     (description "Find immediate dominators in a directed graph.")
     (author (p "Richard Kelsey; ported to Chicken by " 
                (url "http://chicken.wiki.br/users/ivan-raikov" "Ivan Raikov")))

     (history 
      (version "1.4" "Added matchable to list of required eggs")
      (version "1.3" "Ported to Chicken 4")
      (version "1.0" "Initial release"))

     (requires)

     (download "graph-dominators.egg")

     (usage "(require-extension graph-dominators)")

     (documentation
      

      (p "In a control-flow graph (CFG), block M dominates block N if every path "
	 "from the entry that reaches block N has to pass through block M. "
	 "The entry block dominates all blocks.")

      (p "Block M immediately dominates block N if M dominates N, and there is no "
	 "intervening block P such that M dominates P and P dominates N. In other words, "
	 "M is the last dominator on any path from entry to N. Each block has a unique "
	 "immediate dominator, if it has any at all. ")

      (p "This fast dominator code is based upon Lengauer and Tarjan, " 
	 (i "A Fast Algorithm for Finding Dominators in a Flowgraph") ", " 
	 "ACM TOPLAS 1:1, pp. 121--141, July 1979. "
	 "It runs in time " (tt "O(|E|\\log|V|)") ", where " (tt "|E|") 
	 "is the number of edges and " (tt "|V|") " is the number of vertices. "
	 "A smaller time bound of " (tt "O(|E|\\alpha(|E|,|V|))") ", where "
	 (tt "alpha") " is the inverse of Ackerman's function, can be achieved "
	 "with more complex versions of the internal " (tt "link!") " and " (tt "eval!") 
	 " procedures. ")
;
      (subsection "Procedures"

      (procedure "graph-find-dominators-quickly!:: G -> UNDEFINED"
		 
		 (p "Computes the dominator tree of the given rooted, directed graph. "
		    "When done, the meta-data field of each node will contain its immediate dominator. "
		    "It assumes that the meta-data slot for each node contains a list, and modifies "
		    "the car of that list. Requires that the list initially contain " (tt "#f") 
		    " as its car value. "))


      (procedure "graph-find-dominators-slowly!:: G -> UNDEFINED"
		 
		 (p "The fast dominator algorithm is difficult to prove correct, so this procedure " 
		    "is provided in order to check its results.  The slow algorithm, which runs in "
		    "time " (tt "O(|E||V|)") ", is adapted from Aho and Ullman, " 
		    (i "The Theory of Parsing, Translation, and Compiling") ", Prentice-Hall, 1973, "
		    "p. 916. ")))


     (examples (pre #<<EOF
;; example adapted from graph example in the Boost library documentation
(require-extension srfi-1)
(require-extension digraph)
(require-extension graph-dominators)

(define g (make-digraph 'depgraph "dependency graph"))

(define used-by
   (list 
     (cons 'dax_h 'foo_cpp) (cons 'dax_h 'bar_cpp) (cons 'dax_h 'yow_h)
     (cons 'yow_h 'bar_cpp) (cons 'yow_h 'zag_cpp) (cons 'boz_h 'bar_cpp)
     (cons 'boz_h 'zig_cpp) (cons 'boz_h 'zag_cpp) 
     (cons 'foo_cpp 'foo_o) (cons 'foo_o 'libfoobar_a) 
     (cons 'bar_cpp 'bar_o) (cons 'bar_o 'libfoobar_a) 
     (cons 'libfoobar_a 'libzigzag_a)  (cons 'zig_cpp 'zig_o) 
     (cons 'zig_o 'libzigzag_a) (cons 'libfoobar_a 'dax_h) (cons 'zag_cpp 'zag_o) 
     (cons 'zag_o 'libzigzag_a) (cons 'libzigzag_a 'killerapp)))


(define node-list (delete-duplicates 
		   (concatenate (list (map car used-by) (map cdr used-by)))))

(define node-ids (list-tabulate (length node-list) values))
 
(for-each (lambda (i n) ((g 'add-node!) i (list #f n))) node-ids node-list)
(define node-map (zip node-list node-ids))

(for-each (lambda (e) 
	    (match e ((ni . nj) (let ((i (car (alist-ref ni node-map)))
				      (j (car (alist-ref nj node-map))))
				  ((g 'add-edge!) (list i j (format "~A->~A" ni nj)))))
		   (else (error "invalid edge " e))))
	  used-by)

(graph-find-dominators-quickly! g)


EOF
))
     (license
      "Copyright (c) 1993-1999 by Richard Kelsey.  

Mark Reinhold (mbr@research.nj.nec.com)/3 February 1995
Debugging code removed and everything reluctantly Scheme-ized by
R. Kelsey, St. Valentine's Day, 1995

Ported to Chicken Scheme by Ivan Raikov.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
   
     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote 
        products derived from this software without specific prior written 
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER 
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
)))))

(if (eggdoc->html doc) (void))
