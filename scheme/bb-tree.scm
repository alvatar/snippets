;;; -*- Mode: Scheme -*-

;;;; Binary Trees of Bounded Balance

;;; Copyright (c) 2009, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; This file implements maps by binary trees of bounded balance,
;;; described in
;;;
;;;   J. Nievergelt and E.M. Reingold, `Binary search trees of bounded
;;;   balance', ACM SIGACT, 1972.
;;;
;;;   S. Adams, `Implementing Sets Efficiently in a Functional
;;;   Language', Technical Report CSTR 92-10, University of
;;;   Southampton.
;;;
;;; This code depends on SRFI 9 (DEFINE-RECORD-TYPE), SRFI 8 (RECEIVE),
;;; and SRFI 23 (ERROR).
;;;
;;; Documentation is forthcoming.

(define-record-type <bb-tree-type>
    (%make-bb-tree-type key-comparator
                        key-order-predicate
                        operation/find-node)
    bb-tree-type?
  (key-comparator bb-tree-type.key-comparator)
  (key-order-predicate bb-tree-type.key-order-predicate)
  (operation/find-node bb-tree-type.operation/find-node))

(define-record-type <bb-tree>
    (%make-bb-tree type root)
    bb-tree?
  (type bb-tree.type)
  (root bb-tree.root))

(define (make-bb-tree type)
  (%make-bb-tree type (non-node)))

(define (bb-tree/type bb-tree)
  (bb-tree.type bb-tree))

(define (bb-tree/count bb-tree)
  (node/count (bb-tree.root bb-tree)))

(define (bb-tree/empty? bb-tree)
  (not (node? (bb-tree.root bb-tree))))

(define (bb-tree-type/key-order-predicate bb-tree-type)
  (bb-tree-type.key-order-predicate bb-tree-type))

(define (bb-tree-type/key-comparator bb-tree-type)
  (bb-tree-type.key-comparator bb-tree-type))

(define (error:not-comparison object)
  (error "Not a comparison:" object))

(define (error:not-alist object)
  (error "Not an alist:" object))

(define (bb-trees/type bb-tree-a bb-tree-b)
  (let ((type (bb-tree.type bb-tree-a)))
    (if (not (eq? type (bb-tree.type bb-tree-b)))
        (error "BB-trees with mismatched types:" bb-tree-a bb-tree-b))
    type))

(define (bb-tree/find-node bb-tree key)
  ((bb-tree-type.operation/find-node (bb-tree.type bb-tree))
   (bb-tree.root bb-tree)
   key))

(define (bb-tree/search bb-tree key if-found if-not-found)
  (let ((node (bb-tree/find-node bb-tree key)))
    (if (node? node)
        (if-found (node.datum node))
        (if-not-found))))

(define (bb-tree/lookup bb-tree key default)
  (let ((node (bb-tree/find-node bb-tree key)))
    (if (node? node)
        (node.datum node)
        default)))

(define (bb-tree/member? bb-tree key)
  (node? (bb-tree/find-node bb-tree key)))

;;; We can choose a more efficient search algorithm to minimize the
;;; average number of comparisons depending on whether we have an order
;;; predicate or a comparator, provided that it not be defined in terms
;;; of the other construct.  If you have both handy, just use the
;;; comparator: it's better overall, and it's what most of the rest of
;;; the bb-tree implementation uses.

(define (make-bb-tree-type/order-predicate key<?)
  (define (compare-keys key-a key-b)
    (cond ((key<? key-a key-b) -1)
          ((key<? key-b key-a) +1)
          (else 0)))
  (define (find-node node key)
    (let loop ((node node) (candidate #f))
      (if (node? node)
          (if (key<? key (node.key node))
              (loop (node.left node) candidate)
              (loop (node.right node) node))
          (if (or (not candidate) (key<? (node.key candidate) key))
              (non-node)
              candidate))))
  (%make-bb-tree-type compare-keys key<? find-node))

(define (make-bb-tree-type/comparator compare-keys)
  (define (key<? key-a key-b)
    (let ((comparison (compare-keys key-a key-b)))
      (case comparison
        ((-1) #t)
        ((0 +1) #f)
        (else (error:not-comparison comparison)))))
  (define (find-node node key)
    (let loop ((node node))
      (if (node? node)
          (let ((comparison (compare-keys key (node.key node))))
            (case comparison
              ((-1) (loop (node.left node)))
              ((+1) (loop (node.right node)))
              ((0) node)
              (else (error:not-comparison comparison))))
          (non-node))))
  (%make-bb-tree-type compare-keys key<? find-node))

;;; Convention: Every internal procedure respects a left-to-right
;;; ordering compatible with the ordering on the tree in question.  For
;;; example, if a node parameter is to the left of a key parameter, or
;;; if a procedure returns a node to the left of a key, then every key
;;; in the node precedes the key.

(define-record-type <node>
    (%make-node count left key datum right)
    node?
  (count node.count)
  (left node.left)
  (key node.key)
  (datum node.datum)
  (right node.right))

(define (non-node)
  #f)

(define (node-components* n)
  (values (node.count n)
          (node.left n)
          (node.key n)
          (node.datum n)
          (node.right n)))

(define (node-components n)
  (values (node.left n) (node.key n) (node.datum n) (node.right n)))

(define (make-leaf key datum)
  (%make-node 1 (non-node) key datum (non-node)))

;;; Assumption: LEFT and RIGHT are balanced internally, and the two are
;;; balanced with respect to one another.
;;;
;;; Sometimes we use %MAKE-NODE, rather than MAKE-NODE, when we have
;;; already fetched the sizes of the left and right branches, because
;;; that measurably improves performance.

(define (make-node left key datum right)
  (%make-node (+ (node/count left) 1 (node/count right)) left key datum right))

(define (node/count node)
  (if (node? node)
      (node.count node)
      0))

(define (bb-tree/check tree fail)
  (let recur ((node (bb-tree.root tree)))
    (if (node? node)
        (receive (c l k d r) (node-components* node)
          k d                           ;ignore
          (let ((left-count (recur l))
                (right-count (recur r)))
            (if (not (or (<= (+ left-count right-count) 1)
                         (and (<= left-count (* (balance:w) right-count))
                              (<= right-count (* (balance:w) left-count)))))
                (fail "BB-Tree Check -- Balance violation:" node))
            (if (not (= c (+ left-count 1 right-count)))
                (fail "BB-Tree Check -- Count violation:"
                      node c left-count right-count))
            c))
        (begin
          (if (not (eq? node (non-node)))
              (fail "BB-Tree Check -- Neither node nor non-node:" node))
          0))))

;;; Adams' paper claims that if alpha is 1/2, then w must exceed about
;;; 4.646.  However, empirically, a random sequence of insertions
;;; followed by a sequence of minimum deletions reliably yields an
;;; unbalanced tree for me when w is 5.  Setting w to 4 does not.

(define (balance:w) 4)
(define (balance:1/alpha) 2)

;;; Assumption: LEFT and RIGHT are each balanced internally, and the
;;; two are unbalanced by at most one node with respect to one another.

(define (join-balanced left key datum right)
  (let ((lc (node/count left))
        (rc (node/count right)))
    (cond ((<= (+ lc rc) 1) (%make-node (+ lc 1 rc) left key datum right))
          ((<= (* (balance:w) lc) rc) (rotate-left left lc key datum right))
          ((<= (* (balance:w) rc) lc) (rotate-right left key datum right rc))
          (else (%make-node (+ lc 1 rc) left key datum right)))))

(define (rotate-left left lc key datum right)
  (receive (rl rk rd rr) (node-components right)
    (let ((rlc (node/count rl)) (rrc (node/count rr)))
      (if (< rlc (* (balance:1/alpha) rrc))
          (%make-node (+ lc 1 rlc 1 rrc)
                      (%make-node (+ lc 1 rlc) left key datum rl)
                      rk
                      rd
                      rr)
          (receive (rll rlk rld rlr) (node-components rl)
            (let ((rllc (node/count rll)) (rlrc (node/count rlr)))
              (let ((left-count (+ lc 1 rllc))
                    (right-count (+ rlrc 1 rrc)))
                (%make-node (+ left-count 1 right-count)
                            (%make-node left-count left key datum rll)
                            rlk
                            rld
                            (%make-node right-count rlr rk rd rr)))))))))

(define (rotate-right left key datum right rc)
  (receive (ll lk ld lr) (node-components left)
    (let ((llc (node/count ll)) (lrc (node/count lr)))
      (if (< lrc (* (balance:1/alpha) llc))
          (%make-node (+ llc 1 lrc 1 rc)
                      ll
                      lk
                      ld
                      (%make-node (+ lrc 1 rc) lr key datum right))
          (receive (lrl lrk lrd lrr) (node-components lr)
            (let ((lrlc (node/count lrl)) (lrrc (node/count lrr)))
              (let ((left-count (+ llc 1 lrlc))
                    (right-count (+ lrrc 1 rc)))
                (%make-node
                 (+ left-count 1 right-count)
                 (%make-node left-count ll lk ld lrl)
                 lrk
                 lrd
                 (%make-node right-count lrr key datum right)))))))))

;;; Assumption: LEFT and RIGHT are each balanced internally.

(define (join left key datum right)
  (cond ((not (node? left)) (insert-minimum key datum right))
        ((not (node? right)) (insert-maximum left key datum))
        (else
         (receive (lc ll lk ld lr) (node-components* left)
           (receive (rc rl rk rd rr) (node-components* right)
             (cond ((<= (* (balance:w) lc) rc)
                    (join-balanced (join left key datum rl) rk rd rr))
                   ((<= (* (balance:w) rc) lc)
                    (join-balanced ll lk ld (join lr key datum right)))
                   (else
                    (%make-node (+ lc 1 rc) left key datum right))))))))

(define (insert-minimum key datum node)
  (let recur ((node node))
    (if (node? node)
        (receive (nl nk nd nr) (node-components node)
          (join-balanced (recur nl) nk nd nr))
        (make-leaf key datum))))

(define (insert-maximum node key datum)
  (let recur ((node node))
    (if (node? node)
        (receive (nl nk nd nr) (node-components node)
          (join-balanced nl nk nd (recur nr)))
        (make-leaf key datum))))

;;; Assumption: LEFT and RIGHT are each balanced.

(define (concatenate left right)
  (cond ((not (node? left)) right)
        ((not (node? right)) left)
        (else
         (receive (lc ll lk ld lr) (node-components* left)
           (receive (rc rl rk rd rr) (node-components* right)
             (cond ((<= (* (balance:w) lc) rc)
                    (join-balanced (concatenate left rl) rk rd rr))
                   ((<= (* (balance:w) rc) lc)
                    (join-balanced ll lk ld (concatenate lr right)))
                   (else
                    (concatenate-balanced left right))))))))

;;; Assumption: LEFT and RIGHT are each balanced internally, and the
;;; two are unbalanced by at most one node with respect to one another.

(define (concatenate-balanced left right)
  (cond ((not (node? left)) right)
        ((not (node? right)) left)
        (else
         (if (< (node.count left) (node.count right))
             (receive (key datum right*) (delete-minimum right)
               (join-balanced left key datum right*))
             (receive (left* key datum) (delete-maximum left)
               (join-balanced left* key datum right))))))

(define (delete-minimum node)
  (receive (left key datum right) (node-components node)
    (if (node? left)
        (receive (key* datum* left*) (delete-minimum left)
          (values key* datum* (join-balanced left* key datum right)))
        (values key datum right))))

(define (delete-maximum node)
  (receive (left key datum right) (node-components node)
    (if (node? right)
        (receive (right* key* datum*) (delete-maximum right)
          (values (join-balanced left key datum right*) key* datum*))
        (values left key datum))))

(define (bb-tree/update bb-tree key if-found if-not-found)
  (let* ((type (bb-tree.type bb-tree))
         (compare-keys (bb-tree-type.key-comparator type)))
    (let loop
        ((node (bb-tree.root bb-tree))
         (replace (lambda (node) (%make-bb-tree type node)))
         (rebalance (lambda (node) (%make-bb-tree type node))))
      (if (node? node)
          (receive (l k d r) (node-components node)
            (let ((comparison (compare-keys key k)))
              (case comparison
                ((-1)
                 (loop l
                       (lambda (l) (replace (make-node l k d r)))
                       (lambda (l) (rebalance (join-balanced l k d r)))))
                ((+1)
                 (loop r
                       (lambda (r) (replace (make-node l k d r)))
                       (lambda (r) (rebalance (join-balanced l k d r)))))
                ((0)
                 (if-found
                  d
                  (lambda (datum) (replace (make-node l key datum r)))
                  (lambda () (rebalance (concatenate-balanced l r)))))
                (else
                 (error:not-comparison comparison)))))
          (if-not-found (lambda (datum) (rebalance (make-leaf key datum))))))))

;; (define (bb-tree/insert bb-tree key datum)
;;   (bb-tree/update bb-tree key
;;     (lambda (datum* replace delete)
;;       datum* delete                     ;ignore
;;       (replace datum))
;;     (lambda (insert)
;;       (insert datum))))

(define (bb-tree/modify bb-tree key default modifier)
  (bb-tree/update bb-tree key
    (lambda (datum replace delete)
      delete                            ;ignore
      (replace (modifier datum)))
    (lambda (insert)
      (insert (modifier default)))))

(define (bb-tree/intern bb-tree key generator)
  (bb-tree/update bb-tree key
    (lambda (datum replace delete)
      replace delete                    ;ignore
      (values datum bb-tree))
    (lambda (insert)
      (let ((datum (generator key)))
        (values datum (insert datum))))))

;; (define (bb-tree/delete bb-tree key)
;;   (bb-tree/update bb-tree key
;;     (lambda (datum replace delete)
;;       datum replace                     ;ignore
;;       (delete))
;;     (lambda (insert)
;;       insert                            ;ignore
;;       bb-tree)))

;;; Special cases for insertion and deletion yield small but measurable
;;; performance improvements, even though they cause some superfluous
;;; balancing checks to be done in the case of insertion of an existing
;;; key or the case of deletion of a non-existing key.  I have not
;;; compared speed of special cases for modification and internment.

(define (bb-tree/insert bb-tree key datum)
  (let* ((type (bb-tree.type bb-tree))
         (compare-keys (bb-tree-type.key-comparator type)))
    (%make-bb-tree
     type
     (let recur ((node (bb-tree.root bb-tree)))
       (if (node? node)
           (receive (c l k d r) (node-components* node)
             (let ((comparison (compare-keys key k)))
               (case comparison
                 ((-1) (join-balanced (recur l) k d r))
                 ((+1) (join-balanced l k d (recur r)))
                 ((0) (%make-node c l k datum r))
                 (else (error:not-comparison comparison)))))
           (make-leaf key datum))))))

(define (bb-tree/delete bb-tree key)
  (let* ((type (bb-tree.type bb-tree))
         (compare-keys (bb-tree-type.key-comparator type)))
    (%make-bb-tree
     type
     (let recur ((node (bb-tree.root bb-tree)))
       (if (node? node)
           (receive (l k d r) (node-components node)
             (let ((comparison (compare-keys key k)))
               (case comparison
                 ((-1) (join-balanced (recur l) k d r))
                 ((+1) (join-balanced l k d (recur r)))
                 ((0) (concatenate-balanced l r))
                 (else (error:not-comparison comparison)))))
           (non-node))))))

;;;; Minimum and Maximum

(define (bb-tree/min bb-tree default)
  (bb-tree/*min bb-tree key*datum->key default))

(define (bb-tree/min-datum bb-tree default)
  (bb-tree/*min bb-tree key*datum->datum default))

(define (bb-tree/min-pair bb-tree)
  (bb-tree/*min bb-tree key*datum->pair #f))

(define (bb-tree/max bb-tree default)
  (bb-tree/*max bb-tree key*datum->key default))

(define (bb-tree/max-datum bb-tree default)
  (bb-tree/*max bb-tree key*datum->datum default))

(define (bb-tree/max-pair bb-tree)
  (bb-tree/*max bb-tree key*datum->pair #f))

(define (key*datum->key key datum) datum key)
(define (key*datum->datum key datum) key datum)
(define (key*datum->pair key datum) (cons key datum))
(define (key*datum->values key datum) (values key datum))

(define (bb-tree/*min bb-tree selector default)
  (let ((root (bb-tree.root bb-tree)))
    (if (node? root)
        (let loop ((node root))
          (let ((left (node.left node)))
            (if (node? left)
                (loop left)
                (selector (node.key node) (node.datum node)))))
        default)))

(define (bb-tree/*max bb-tree selector default)
  (let ((root (bb-tree.root bb-tree)))
    (if (node? root)
        (let loop ((node root))
          (let ((right (node.right node)))
            (if (node? right)
                (loop right)
                (selector (node.key node) (node.datum node)))))
        default)))

;;;;; Deleting the Minimum and Maximum

(define (bb-tree/delete-min bb-tree default)
  (bb-tree/*delete-min bb-tree key*datum->key default))

(define (bb-tree/delete-min-datum bb-tree default)
  (bb-tree/*delete-min bb-tree key*datum->datum default))

(define (bb-tree/delete-min-pair bb-tree)
  (bb-tree/*delete-min bb-tree key*datum->pair #f))

(define (bb-tree/delete-max bb-tree default)
  (bb-tree/*delete-max bb-tree key*datum->key default))

(define (bb-tree/delete-max-datum bb-tree default)
  (bb-tree/*delete-max bb-tree key*datum->datum default))

(define (bb-tree/delete-max-pair bb-tree)
  (bb-tree/*delete-max bb-tree key*datum->pair #f))

(define (bb-tree/*delete-min bb-tree selector default)
  (let ((root (bb-tree.root bb-tree)))
    (if (node? root)
        (let ((type (bb-tree.type bb-tree)))
          (receive (key datum root) (delete-minimum (bb-tree.root bb-tree))
            (values (selector key datum) (%make-bb-tree type root))))
        (values default bb-tree))))

(define (bb-tree/*delete-max bb-tree selector default)
  (let ((root (bb-tree.root bb-tree)))
    (if (node? root)
        (let ((type (bb-tree.type bb-tree)))
          (receive (root key datum) (delete-maximum (bb-tree.root bb-tree))
            (values (selector key datum) (%make-bb-tree type root))))
        (values default bb-tree))))

;;;;; Updating the Minimum and Maximum

(define (bb-tree/update-min bb-tree if-found if-empty)
  (let ((root (bb-tree.root bb-tree)))
    (if (node? root)
        (let* ((type (bb-tree.type bb-tree))
               (replace-root (lambda (node) (%make-bb-tree type node))))
          (let loop
              ((node root) (replace replace-root) (rebalance replace-root))
            (receive (l k d r) (node-components node)
              (if (node? l)
                  (loop l
                        (lambda (l) (replace (make-node l k d r)))
                        (lambda (l) (rebalance (join-balanced l k d r))))
                  (if-found k
                            d
                            (lambda (d) (replace (make-node l k d r)))
                            (lambda () (rebalance r)))))))
        (if-empty))))

(define (bb-tree/update-max bb-tree if-found if-empty)
  (let ((root (bb-tree.root bb-tree)))
    (if (node? root)
        (let* ((type (bb-tree.type bb-tree))
               (replace-root (lambda (node) (%make-bb-tree type node))))
          (let loop
              ((node root) (replace replace-root) (rebalance replace-root))
            (receive (l k d r) (node-components node)
              (if (node? r)
                  (loop r
                        (lambda (r) (replace (make-node l k d r)))
                        (lambda (r) (rebalance (join-balanced l k d r))))
                  (if-found k
                            d
                            (lambda (d) (replace (make-node l k d r)))
                            (lambda () (rebalance l)))))))
        (if-empty))))

;;;; Indexing

(define (bb-tree/rank bb-tree key)
  (bb-tree/key->index bb-tree key))

(define (bb-tree/key->index bb-tree key)
  (let ((compare-keys (bb-tree-type.key-comparator (bb-tree.type bb-tree))))
    (let loop ((node (bb-tree.root bb-tree)) (offset 0))
      (if (node? node)
          (receive (l k d r) (node-components node)
            d                           ;ignore
            (let ((comparison (compare-keys key k)))
              (case comparison
                ((-1) (loop l offset))
                ((+1) (loop r (+ offset (node/count l) 1)))
                ((0) (+ offset (node/count l)))
                (else (error:not-comparison comparison)))))
          #f))))

(define (bb-tree/index bb-tree index)
  (bb-tree/*index bb-tree index key*datum->key))

(define (bb-tree/index-datum bb-tree index)
  (bb-tree/*index bb-tree index key*datum->datum))

(define (bb-tree/index-pair bb-tree index)
  (bb-tree/*index bb-tree index key*datum->pair))

(define (bb-tree/index-key&datum bb-tree index)
  (bb-tree/*index bb-tree index key*datum->values))

(define (bb-tree/*index bb-tree index selector)
  (let ((root (bb-tree.root bb-tree)))
    (let ((count (node/count root)))
      (if (or (< index 0) (<= count index))
          (error "Index out of bounds:" index bb-tree)))
    ;; The above test, and the loop structure below together with the
    ;; tree structure, all guarantee that NODE is a non-empty node at
    ;; entry to the loop.
    (let loop ((node root) (index index))
      (receive (left key datum right) (node-components node)
        (let ((left-count (node/count left)))
          (cond ((< index left-count) (loop left index))
                ((< left-count index) (loop right (- index (+ left-count 1))))
                (else (selector key datum))))))))

;;;;; Deleting and Replacing at Indices

(define (bb-tree/delete-index bb-tree index)
  (bb-tree/*delete-index bb-tree index key*datum->key))

(define (bb-tree/delete-index-datum bb-tree index)
  (bb-tree/*delete-index bb-tree index key*datum->datum))

(define (bb-tree/delete-index-pair bb-tree index)
  (bb-tree/*delete-index bb-tree index key*datum->pair))

(define (bb-tree/delete-index-key&datum bb-tree index)
  (receive (pair bb-tree) (bb-tree/delete-index-pair bb-tree index)
    (values (car pair) (cdr pair) bb-tree)))

(define (bb-tree/*delete-index bb-tree index selector)
  (let ((type (bb-tree.type bb-tree))
        (root (bb-tree.root bb-tree)))
    (let ((count (node/count root)))
      (if (or (< index 0) (<= count index))
          (error "Index out of bounds:" index bb-tree)))
    (call-with-values
        (lambda ()
          (let recur ((node root) (index index))
            (receive (left key datum right) (node-components node)
              (let ((left-count (node/count left)))
                (cond ((< index left-count)
                       (receive (result left) (recur left index)
                         (values result (join-balanced left key datum right))))
                      ((< left-count index)
                       (receive (result right)
                                (recur right (- index (+ left-count 1)))
                         (values result (join-balanced left key datum right))))
                      (else
                       (values (selector key datum)
                               (concatenate-balanced left right))))))))
      (lambda (result root)
        (values result (%make-bb-tree type root))))))

;;; Why no INSERT-INDEX?  This is a data structure for a set of
;;; associations that happen to be ordered, not a data structure for a
;;; sequence.  If you have an association, just insert it the usual
;;; way; it will go in the right place.

(define (bb-tree/replace-index bb-tree index datum)
  (let ((type (bb-tree.type bb-tree))
        (root (bb-tree.root bb-tree)))
    (let ((count (node/count root)))
      (if (or (< index 0) (<= count index))
          (error "Index out of bounds:" index bb-tree)))
    (%make-bb-tree
     type
     (let recur ((node root) (index index))
       (receive (count left key datum* right) (node-components* node)
         (let ((left-count (node/count left)))
           (cond ((< index left-count)
                  (%make-node count (recur left index) key datum* right))
                 ((< left-count index)
                  (let ((index (- index (+ left-count 1))))
                    (%make-node count left key datum* (recur right index))))
                 (else
                  (%make-node count left key datum right)))))))))

;;;;; Editing at Indices

(define (bb-tree/modify-index bb-tree index modifier)
  (let ((type (bb-tree.type bb-tree))
        (root (bb-tree.root bb-tree)))
    (let ((count (node/count root)))
      (if (or (< index 0) (<= count index))
          (error "Index out of bounds:" index bb-tree)))
    (%make-bb-tree
     type
     (let recur ((node root) (index index))
       (receive (count left key datum right) (node-components* node)
         (let ((left-count (node/count left)))
           (cond ((< index left-count)
                  (%make-node count (recur left index) key datum right))
                 ((< left-count index)
                  (let ((index (- index (+ left-count 1))))
                    (%make-node count left key datum (recur right index))))
                 (else
                  (let ((datum (modifier key datum)))
                    (%make-node count left key datum right))))))))))

(define (bb-tree/update-index bb-tree index receiver)
  (let ((type (bb-tree.type bb-tree))
        (root (bb-tree.root bb-tree)))
    (let ((count (node/count root)))
      (if (or (< index 0) (<= count index))
          (error "Index out of bounds:" index bb-tree)))
    (let ((replace (lambda (root) (%make-bb-tree type root))))
      (let loop
          ((node root) (index index) (replace replace) (rebalance replace))
        (receive (left key datum right) (node-components node)
          (let ((left-count (node/count left)))
            (cond ((< index left-count)
                   (loop left
                         index
                         (lambda (left)
                           (replace (make-node left key datum right)))
                         (lambda (left)
                           (rebalance (join-balanced left key datum right)))))
                  ((< left-count index)
                   (loop right
                         (- index (+ left-count 1))
                         (lambda (right)
                           (replace (make-node left key datum right)))
                         (lambda (right)
                           (rebalance (join-balanced left key datum right)))))
                  (else
                   (receiver key
                             datum
                             (lambda (datum)
                               (replace (make-node left key datum right)))
                             (lambda ()
                               (rebalance
                                (concatenate-balanced left right))))))))))))

;;;; Splitting on Pivots

;; (define (bb-tree/split bb-tree key)
;;   (values (bb-tree/split< bb-tree key)
;;           (bb-tree/split> bb-tree key)))

(define (bb-tree/split bb-tree key)
  (let* ((type (bb-tree.type bb-tree))
         (compare-keys (bb-tree-type.key-comparator type)))
    (call-with-values
        (lambda ()
          (let recur ((node (bb-tree.root bb-tree)))
            (if (node? node)
                (receive (l k d r) (node-components node)
                  (let ((comparison (compare-keys key k)))
                    (case comparison
                      ((-1) (receive (lesser greater) (recur l)
                              (values lesser (join greater k d r))))
                      ((+1) (receive (lesser greater) (recur r)
                              (values (join l k d lesser) greater)))
                      ((0) (values l r))
                      (else (error:not-comparison comparison)))))
                (values (non-node) (non-node)))))
      (lambda (lesser greater)
        (values (%make-bb-tree type lesser)
                (%make-bb-tree type greater))))))

;; (define (bb-tree/split* bb-tree key if-found if-not-found)
;;   (let ((lesser (bb-tree/split< bb-tree key))
;;         (greater (bb-tree/split> bb-tree key)))
;;     (bb-tree/search bb-tree key
;;       (lambda (datum) (if-found datum lesser greater))
;;       (lambda () (if-not-found lesser greater)))))

(define (bb-tree/split* bb-tree key if-found if-not-found)
  (let* ((type (bb-tree.type bb-tree))
         (compare-keys (bb-tree-type.key-comparator type)))
    (receive (lesser node greater)
             (split* (bb-tree.root bb-tree) key compare-keys)
      (let ((lesser (%make-bb-tree type lesser))
            (greater (%make-bb-tree type greater)))
        (if (node? node)
            (if-found (node.datum node) lesser greater)
            (if-not-found lesser greater))))))

;++ This has a mild space leak.  It should use a different protocol for
;++ returning the node's datum (and possibly its key), rather than just
;++ returning the node (which hangs on to its (possibly large and now
;++ otherwise unreferenced) children).

(define (split* node key compare-keys)
  (let recur ((node node))
    (if (node? node)
        (receive (l k d r) (node-components node)
          (let ((comparison (compare-keys key k)))
            (case comparison
              ((-1) (receive (lesser node greater) (recur l)
                      (values lesser node (join greater k d r))))
              ((+1) (receive (lesser node greater) (recur r)
                      (values (join l k d lesser) node greater)))
              ((0) (values l node r))
              (else (error:not-comparison comparison)))))
        (values (non-node) (non-node) (non-node)))))

(define (bb-tree/split< bb-tree key)
  (let ((type (bb-tree.type bb-tree)))
    (%make-bb-tree
     type
     (split< (bb-tree.root bb-tree) key (bb-tree-type.key-comparator type)))))

(define (bb-tree/split> bb-tree key)
  (let ((type (bb-tree.type bb-tree)))
    (%make-bb-tree
     type
     (split> (bb-tree.root bb-tree) key (bb-tree-type.key-comparator type)))))

(define (split< node key compare-keys)
  (let recur ((node node))
    (if (node? node)
        (receive (l k d r) (node-components node)
          (let ((comparison (compare-keys key k)))
            (case comparison
              ((-1) (recur l))
              ((+1) (join l k d (recur r)))
              ((0) l)
              (else (error:not-comparison comparison)))))
        (non-node))))

(define (split> node key compare-keys)
  (let recur ((node node))
    (if (node? node)
        (receive (l k d r) (node-components node)
          (let ((comparison (compare-keys key k)))
            (case comparison
              ((-1) (join (recur l) k d r))
              ((+1) (recur r))
              ((0) r)
              (else (error:not-comparison comparison)))))
        (non-node))))

;;;; Set Operations

;;; TRIM-BETWEEN and TRIM-BETWEEN/MERGE are the basic operations; the
;;; others are specializations omitting lower or upper bounds.  The
;;; choice of TRIM-ABOVE/MERGE versus TRIM-BELOW/MERGE is arbitrary.

(define (trim-between low high node key<?)
  (let loop ((node node))
    (if (node? node)
        (receive (left key datum right) (node-components node)
          datum                         ;ignore
          (if (key<? low key)
              (if (key<? key high) node (loop left))
              (loop right)))
        node)))

(define (trim-above low node key<?)
  (let loop ((node node))
    (if (node? node)
        (receive (left key datum right) (node-components node)
          left datum                    ;ignore
          (if (key<? low key) node (loop right)))
        node)))

(define (trim-below high node key<?)
  (let loop ((node node))
    (if (node? node)
        (receive (left key datum right) (node-components node)
          datum right                   ;ignore
          (if (key<? key high) node (loop left)))
        node)))

(define (trim-between/merge k d high node merger compare-keys key<? find-node)
  (let loop ((node node))
    (if (node? node)
        (receive (nl nk nd nr) (node-components node)
          (let ((comparison (compare-keys k nk)))
            (case comparison
              ((-1) (if (key<? nk high)
                        (values (let ((n (find-node nl k)))
                                  (if (node? n) (merger k d (node.datum n)) d))
                                node)
                        (loop nl)))
              ((+1) (loop nr))
              ;++ Is TRIM-BETWEEN here necessary, or could we return NR?
              ((0) (values (merger k d nd) (trim-between k high nr key<?)))
              (else (error:not-comparison comparison)))))
        (values d node))))

(define (trim-above/merge k d node merger compare-keys key<? find-node)
  (let loop ((node node))
    (if (node? node)
        (receive (nl nk nd nr) (node-components node)
          (let ((comparison (compare-keys k nk)))
            (case comparison
              ((-1) (values (let ((n (find-node nl k)))
                              (if (node? n) (merger k d (node.datum n)) d))
                            node))
              ((+1) (loop nr))
              ;++ Is TRIM-ABOVE here necessary, or could we return NR?
              ((0) (values (merger k d nd) (trim-above k nr key<?)))
              (else (error:not-comparison comparison)))))
        (values d node))))

;;; Even hairier versions of the above, for the difference operation.

(define (trim-between/search key high node compare-keys key<? find-node
                             if-found if-not-found)
  (let loop ((node node))
    (if (node? node)
        (receive (nl nk nd nr) (node-components node)
          (let ((comparison (compare-keys key nk)))
            (case comparison
              ((-1) (if (key<? nk high)
                        (let ((n (find-node nl key)))
                          (if (node? n)
                              (if-found (node.datum n) node)
                              (if-not-found node)))
                        (loop nl)))
              ((+1) (loop nr))
              ((0) (if-found nd (trim-between key high nr key<?)))
              (else (error:not-comparison comparison)))))
        (if-not-found node))))

(define (trim-above/search key node compare-keys key<? find-node
                           if-found if-not-found)
  (let loop ((node node))
    (if (node? node)
        (receive (nl nk nd nr) (node-components node)
          (let ((comparison (compare-keys key nk)))
            (case comparison
              ((-1) (let ((n (find-node nl key)))
                      (if (node? n)
                          (if-found (node.datum n) node)
                          (if-not-found node))))
              ((+1) (loop nr))
              ((0) (if-found nd (trim-above key nr key<?)))
              (else (error:not-comparison comparison)))))
        (if-not-found node))))

;;;;; Union

(define (union-merge node-a node-b merger compare-keys key<? find)

  (define (union-between low high node-a node-b)
    (if (node? node-b)
        (receive (bl bk bd br) (node-components node-b)
          (if (node? node-a)
              (receive (al ak ad ar) (node-components node-a)
                (receive (ad greater)
                         (trim-between/merge ak ad high node-b
                                             merger compare-keys key<? find)
                  (join
                   (union-between low ak al (trim-between low ak node-b key<?))
                   ak
                   ad
                   (union-between ak high ar greater))))
              (join (split> bl low compare-keys)
                    bk
                    bd
                    (split< br high compare-keys))))
        node-a))

  (define (union-above low node-a node-b)
    (if (node? node-b)
        (receive (bl bk bd br) (node-components node-b)
          (if (node? node-a)
              (receive (al ak ad ar) (node-components node-a)
                (receive (ad greater)
                         (trim-above/merge ak ad node-b
                                           merger compare-keys key<? find)
                  (join
                   (union-between low ak al (trim-between low ak node-b key<?))
                   ak
                   ad
                   (union-above ak ar greater))))
              (join (split> bl low compare-keys) bk bd br)))
        node-a))

  (define (union-below high node-a node-b)
    (if (node? node-b)
        (receive (bl bk bd br) (node-components node-b)
          (if (node? node-a)
              (receive (al ak ad ar) (node-components node-a)
                (receive (ad greater)
                         (trim-between/merge ak ad high node-b
                                             merger compare-keys key<? find)
                  (join (union-below ak al (trim-below ak node-b key<?))
                        ak
                        ad
                        (union-between ak high ar greater))))
              (join bl bk bd (split< br high compare-keys))))
        node-a))

  (cond ((not (node? node-a)) node-b)
        ((not (node? node-b)) node-a)
        (else
         (receive (al ak ad ar) (node-components node-a)
           (receive (ad greater)
                    (trim-above/merge ak ad node-b
                                      merger compare-keys key<? find)
             (join (union-below ak al (trim-below ak node-b key<?))
                   ak
                   ad
                   (union-above ak ar greater)))))))

;;;;; Intersection

;;; This could be specialized to omit the merger procedure parameter.

(define (intersection-merge node-a node-b merger compare-keys)
  (define (merge key datum node flip?)
    (if flip?
        (merger key (node.datum node) datum)
        (merger key datum (node.datum node))))
  (let recur ((node-a node-a) (node-b node-b) (flip? #f))
    (if (and (node? node-a) (node? node-b))
        (if (< (node.count node-a) (node.count node-b))
            (receive (al ak ad ar) (node-components node-a)
              (receive (lesser node greater) (split* node-b ak compare-keys)
                (let ((left (recur al lesser flip?))
                      (right (recur ar greater flip?)))
                  (if (node? node)
                      (join left ak (merge ak ad node flip?) right)
                      (concatenate left right)))))
            (receive (bl bk bd br) (node-components node-b)
              (receive (lesser node greater) (split* node-a bk compare-keys)
                (let ((flip? (not flip?)))
                  (let ((left (recur bl lesser flip?))
                        (right (recur br greater flip?)))
                    (if (node? node)
                        (join left bk (merge bk bd node flip?) right)
                        (concatenate left right)))))))
        (non-node))))

;;;;; Difference

;;; The easy version -- take associations from NODE-A whose keys are
;;; not in NODE-B.

(define (difference node-a node-b compare-keys key<?)

  (define (difference-between low high node-a node-b)
    (if (node? node-a)
        (if (node? node-b)
            (receive (bl bk bd br) (node-components node-b)
              bd                        ;ignore
              (concatenate
               (difference-between low
                                   bk
                                   (trim-between low bk node-a key<?)
                                   bl)
               (difference-between bk
                                   high
                                   (trim-between bk high node-a key<?)
                                   br)))
            (receive (al ak ad ar) (node-components node-a)
              (join (split> al low compare-keys)
                    ak
                    ad
                    (split< ar high compare-keys))))
        node-a))

  (define (difference-above low node-a node-b)
    (if (node? node-a)
        (if (node? node-b)
            (receive (bl bk bd br) (node-components node-b)
              bd                        ;ignore
              (concatenate
               (difference-between low
                                   bk
                                   (trim-between low bk node-a key<?)
                                   bl)
               (difference-above bk (trim-above bk node-a key<?) br)))
            (receive (al ak ad ar) (node-components node-a)
              (join (split> al low compare-keys) ak ad ar)))
        node-a))

  (define (difference-below high node-a node-b)
    (if (node? node-a)
        (if (node? node-b)
            (receive (bl bk bd br) (node-components node-b)
              bd                        ;ignore
              (concatenate
               (difference-below bk (trim-below bk node-a key<?) bl)
               (difference-between bk
                                   high
                                   (trim-between bk high node-a key<?)
                                   br)))
            (receive (al ak ad ar) (node-components node-a)
              (join al ak ad (split< ar high compare-keys))))
        node-a))

  (if (and (node? node-a) (node? node-b))
      (receive (bl bk bd br) (node-components node-b)
        bd                              ;ignore
        (concatenate (difference-below bk (trim-below bk node-a key<?) bl)
                     (difference-above bk (trim-above bk node-a key<?) br)))
      node-a))

;;;;;; Difference with Merging

;;; The hard version -- MERGER chooses what to do when a match is
;;; found: associate some datum (possibly a different one from that in
;;; NODE-A and from that in NODE-B), or delete the association.

(define (difference-merge node-a node-b merger compare-keys key<? find)

  (define (difference-between low high node-a node-b)
    (if (node? node-a)
        (if (node? node-b)
            (receive (bl bk bd br) (node-components node-b)
              (let ((lesser (trim-between low bk node-a key<?)))
                (trim-between/search bk high node-a compare-keys key<? find
                  (lambda (ad greater)
                    (merger bk ad bd
                      (lambda (bd)
                        (join (difference-between low bk lesser bl)
                              bk
                              bd
                              (difference-between bk high greater br)))
                      (lambda ()
                        (concatenate
                         (difference-between low bk lesser bl)
                         (difference-between bk high greater br)))))
                  (lambda (greater)
                    (concatenate (difference-between low bk lesser bl)
                                 (difference-between bk high greater br))))))
            (receive (al ak ad ar) (node-components node-a)
              (join (split> al low compare-keys)
                    ak
                    ad
                    (split< ar high compare-keys))))
        node-a))

  (define (difference-above low node-a node-b)
    (if (node? node-a)
        (if (node? node-b)
            (receive (bl bk bd br) (node-components node-b)
              (let ((lesser (trim-between low bk node-a key<?)))
                (trim-above/search bk node-a compare-keys key<? find
                  (lambda (ad greater)
                    (merger bk ad bd
                      (lambda (bd)
                        (join (difference-between low bk lesser bl)
                              bk
                              bd
                              (difference-above bk greater br)))
                      (lambda ()
                        (concatenate (difference-between low bk lesser bl)
                                     (difference-above bk greater br)))))
                  (lambda (greater)
                    (concatenate (difference-between low bk lesser bl)
                                 (difference-above bk greater br))))))
            (receive (al ak ad ar) (node-components node-a)
              (join (split> al low compare-keys) ak ad ar)))
        node-a))

;;;;;; Difference with Merging, continued

  (define (difference-below high node-a node-b)
    (if (node? node-a)
        (if (node? node-b)
            (receive (bl bk bd br) (node-components node-b)
              (let ((lesser (trim-below bk node-a key<?)))
                (trim-between/search bk high node-a compare-keys key<? find
                  (lambda (ad greater)
                    (merger bk ad bd
                      (lambda (bd)
                        (join (difference-below bk lesser bl)
                              bk
                              bd
                              (difference-between bk high greater br)))
                      (lambda ()
                        (concatenate
                         (difference-below bk lesser bl)
                         (difference-between bk high greater br)))))
                  (lambda (greater)
                    (concatenate (difference-below bk lesser bl)
                                 (difference-between bk high greater br))))))
            (receive (al ak ad ar) (node-components node-a)
              (join al ak ad (split< ar high compare-keys))))
        node-a))

  (if (and (node? node-a) (node? node-b))
      (receive (bl bk bd br) (node-components node-b)
        (let ((lesser (trim-below bk node-a key<?)))
          (trim-above/search bk node-a compare-keys key<? find
            (lambda (ad greater)
              (merger bk ad bd
                (lambda (bd)
                  (join (difference-below bk lesser bl)
                        bk
                        bd
                        (difference-above bk greater br)))
                (lambda ()
                  (concatenate (difference-below bk lesser bl)
                               (difference-above bk greater br)))))
            (lambda (greater)
              (concatenate (difference-below bk lesser bl)
                           (difference-above bk greater br))))))
      node-a))

;;;;; Set Operation Front Ends

(define (bb-tree/union bb-tree-a bb-tree-b)
  (bb-tree/left-union bb-tree-a bb-tree-b))

(define (bb-tree/left-union bb-tree-a bb-tree-b)
  (bb-tree/union-merge bb-tree-a bb-tree-b merge-left))

(define (bb-tree/right-union bb-tree-a bb-tree-b)
  (bb-tree/union-merge bb-tree-a bb-tree-b merge-right))

(define (bb-tree/union-merge bb-tree-a bb-tree-b merger)
  (let ((type (bb-trees/type bb-tree-a bb-tree-b)))
    (%make-bb-tree type
                   (union-merge (bb-tree.root bb-tree-a)
                                (bb-tree.root bb-tree-b)
                                merger
                                (bb-tree-type.key-comparator type)
                                (bb-tree-type.key-order-predicate type)
                                (bb-tree-type.operation/find-node type)))))

(define (bb-tree/intersection bb-tree-a bb-tree-b)
  (bb-tree/left-intersection bb-tree-a bb-tree-b))

(define (bb-tree/left-intersection bb-tree-a bb-tree-b)
  (bb-tree/intersection-merge bb-tree-a bb-tree-b merge-left))

(define (bb-tree/right-intersection bb-tree-a bb-tree-b)
  (bb-tree/intersection-merge bb-tree-a bb-tree-b merge-right))

(define (bb-tree/intersection-merge bb-tree-a bb-tree-b merger)
  (let ((type (bb-trees/type bb-tree-a bb-tree-b)))
    (%make-bb-tree
     type
     (intersection-merge (bb-tree.root bb-tree-a)
                         (bb-tree.root bb-tree-b)
                         merger
                         (bb-tree-type.key-comparator type)))))

(define (merge-left key datum-a datum-b)
  key datum-b                           ;ignore
  datum-a)

(define (merge-right key datum-a datum-b)
  key datum-a                           ;ignore
  datum-b)

(define (bb-tree/difference bb-tree-a bb-tree-b)
  (let ((type (bb-trees/type bb-tree-a bb-tree-b)))
    (%make-bb-tree type
                   (difference (bb-tree.root bb-tree-a)
                               (bb-tree.root bb-tree-b)
                               (bb-tree-type.key-comparator type)
                               (bb-tree-type.key-order-predicate type)))))

(define (bb-tree/difference-merge bb-tree-a bb-tree-b merger)
  (let ((type (bb-trees/type bb-tree-a bb-tree-b)))
    (%make-bb-tree
     type
     (difference-merge (bb-tree.root bb-tree-a)
                       (bb-tree.root bb-tree-b)
                       merger
                       (bb-tree-type.key-comparator type)
                       (bb-tree-type.key-order-predicate type)
                       (bb-tree-type.operation/find-node type)))))

;;;; Set Relations

;;; This probably admits a clever hedge-based algorithm too.

(define (submap? node-a node-b datum=? compare-keys)
  (let recur ((node-a node-a) (node-b node-b))
    (or (not (node? node-a))
        (and (node? node-b)
             (<= (node.count node-a) (node.count node-b))
             (receive (al ak ad ar) (node-components node-a)
               (receive (lesser node greater) (split* node-b ak compare-keys)
                 (and node
                      (datum=? ad (node.datum node))
                      (recur al lesser)
                      (recur ar greater))))))))

;; (define (submap? node-a node-b datum=? member? compare-keys)
;;   (let recur ((node-a node-a) (node-b node-b))
;;     (or (not (node? node-a))
;;         (and (node? node-b)
;;              (<= (node.count node-a) (node.count node-b))
;;              (receive (al ak ad ar) (node-components node-a)
;;                (receive (bl bk bd br) (node-components node-b)
;;                  (let ((comparison (compare-keys ak bk)))
;;                    (case comparison
;;                      ((-1) (and (recur al bl)
;;                                 (member? ak node-b)
;;                                 (recur ar node-b)))
;;                      ((+1) (and (recur ar br)
;;                                 (member? ak node-b)
;;                                 (recur al node-b)))
;;                      ((0) (and (recur al bl) (recur ar br)))
;;                      (else (error:not-comparison comparison))))))))))

;++ Should DATUM=? be sensitive to the key?

(define (bb-tree/submap? bb-tree-a bb-tree-b datum=?)
  (submap? (bb-tree.root bb-tree-a)
           (bb-tree.root bb-tree-b)
           datum=?
           (bb-tree-type.key-comparator (bb-trees/type bb-tree-a bb-tree-b))))

(define (bb-tree/subset? bb-tree-a bb-tree-b)
  (bb-tree/submap? bb-tree-a bb-tree-b (lambda (a b) a b #t)))

;;;; Filter, Map, and Fold

;;; Are these uber-general composite operations useful?

(define (bb-tree/filter-map-fold-ascending bb-tree initial-value procedure)
  (let ((type (bb-tree.type bb-tree)))
    (call-with-values
        (lambda ()
          (let recur ((node (bb-tree.root bb-tree)) (value initial-value))
            (if (node? node)
                (receive (l k d r) (node-components node)
                  (receive (value l) (recur l value)
                    ;; Want multi-continuation procedure calls!
                    (procedure k d value
                      (lambda (d value)
                        (receive (value r) (recur r value)
                          (values value (join l k d r))))
                      (lambda (value)
                        (receive (value r) (recur r value)
                          (values value (concatenate l r)))))))
                (values value node))))
      (lambda (value node)
        (values value (%make-bb-tree type node))))))

(define (bb-tree/filter-map-fold-descending bb-tree initial-value procedure)
  (let ((type (bb-tree.type bb-tree)))
    (call-with-values
        (lambda ()
          (let recur ((node (bb-tree.root bb-tree)) (value initial-value))
            (if (node? node)
                (receive (l k d r) (node-components node)
                  (receive (value r) (recur r value)
                    ;; Want multi-continuation procedure calls!
                    (procedure k d value
                      (lambda (d value)
                        (receive (value l) (recur l value)
                          (values value (join l k d r))))
                      (lambda (value)
                        (receive (value l) (recur l value)
                          (values value (concatenate l r)))))))
                (values value node))))
      (lambda (value node)
        (values value (%make-bb-tree type node))))))

;;; Various special cases of the above.

;; (define (bb-tree/filter bb-tree predicate)
;;   (bb-tree/filter-map bb-tree
;;     (lambda (key datum replace delete)
;;       (if (predicate key datum)
;;           (replace datum)
;;           (delete)))))

;; (define (bb-tree/filter-map bb-tree procedure)
;;   (receive (dummy bb-tree)
;;            (bb-tree/filter-map-fold bb-tree 'DUMMY
;;              (lambda (dummy key datum replace delete)
;;                dummy                    ;ignore
;;                (procedure key datum
;;                  (lambda (datum) (replace datum 'DUMMY))
;;                  (lambda () (delete 'DUMMY)))))
;;     dummy                               ;ignore
;;     bb-tree))

;; (define (bb-tree/map bb-tree procedure)
;;   (bb-tree/filter-map bb-tree
;;     (lambda (key datum replace delete)
;;       delete                            ;ignore
;;       (replace (procedure key datum)))))

;;; These definitions may be faster, but I haven't compared their
;;; performance.

(define (bb-tree/filter bb-tree predicate)
  (%make-bb-tree
   (bb-tree.type bb-tree)
   (let recur ((node (bb-tree.root bb-tree)))
     (if (node? node)
         (receive (l k d r) (node-components node)
           (let ((l (recur l)) (r (recur r)))
             (if (predicate k d)
                 (join l k d r)
                 (concatenate l r))))
         (non-node)))))

(define (bb-tree/filter-map bb-tree procedure)
  (%make-bb-tree
   (bb-tree.type bb-tree)
   (let recur ((node (bb-tree.root bb-tree)))
     (if (node? node)
         (receive (l k d r) (node-components node)
           (let ((l (recur l)) (r (recur r)))
             (procedure k d
               (lambda (d) (join l k d r))
               (lambda () (concatenate l r)))))
         (non-node)))))

(define (bb-tree/map bb-tree procedure)
  (%make-bb-tree
   (bb-tree.type bb-tree)
   (let recur ((node (bb-tree.root bb-tree)))
     (if (node? node)
         (receive (c l k d r) (node-components* node)
           ;; %MAKE-NODE is safe here because the counts are left
           ;; unchanged.
           (%make-node c (recur l) k (procedure k d) (recur r)))
         (non-node)))))

;;;;; Folding and Listing

(define (bb-tree/fold-ascending bb-tree initial-value combinator)
  (let ((root (bb-tree.root bb-tree)))
    (if (node? root)
        (let recur ((node root) (value initial-value))
          (receive (l k d r) (node-components node)
            (let ((value (if (node? l) (recur l value) value)))
              (if (node? r)
                  (recur r (combinator k d value))
                  (combinator k d value)))))
        initial-value)))

(define (bb-tree/fold-descending bb-tree initial-value combinator)
  (let ((root (bb-tree.root bb-tree)))
    (if (node? root)
        (let recur ((node root) (value initial-value))
          (receive (l k d r) (node-components node)
            (let ((value (if (node? r) (recur r value) value)))
              (if (node? l)
                  (recur l (combinator k d value))
                  (combinator k d value)))))
        initial-value)))

(define (bb-tree->list bb-tree selector)
  (bb-tree/fold-descending bb-tree '()
    (lambda (key datum alist)
      (cons (selector key datum) alist))))

(define (bb-tree->alist bb-tree)
  (bb-tree->list bb-tree cons))

(define (bb-tree/key-list bb-tree)
  (bb-tree->list bb-tree (lambda (key datum) datum key)))

(define (bb-tree/datum-list bb-tree)
  (bb-tree->list bb-tree (lambda (key datum) key datum)))

(define (alist->bb-tree alist type)
  (let loop ((alist alist) (bb-tree (make-bb-tree type)))
    (if (pair? alist)
        (loop (cdr alist) (bb-tree/insert bb-tree (caar alist) (cdar alist)))
        (begin
          (if (not (null? alist))
              (error:not-alist alist))
          bb-tree))))

;; ;;;; BB-Tree <-> Stream
;;
;; (define (bb-tree->stream bb-tree)
;;   (let recur ((node (bb-tree.root bb-tree)) (tail stream-nil))
;;     (lazy (if (node? node)
;;               (receive (l k d r) (node-components node)
;;                 (recur l (stream-cons (cons k d) (recur r tail))))
;;               tail))))
;;
;; (define (stream->bb-tree stream bb-tree-type)
;;   (let loop ((stream stream) (bb-tree (make-bb-tree bb-tree-type)))
;;     (if (stream-pair? stream)
;;         (loop (stream-cdr stream)
;;               (let ((association (stream-car stream)))
;;                 (bb-tree/insert bb-tree
;;                                 (car association)
;;                                 (cdr association))))
;;         bb-tree)))
;;
;; (define (bb-tree/equal? bb-tree-a bb-tree-b datum=?)
;;   (let ((compare-keys
;;          (bb-tree-type.key-comparator (bb-trees/type bb-tree-a bb-tree-b))))
;;     (let loop ((stream-a (bb-tree->stream bb-tree-a))
;;                (stream-b (bb-tree->stream bb-tree-b)))
;;       (if (stream-pair? stream-a)
;;           (and (stream-pair? stream-b)
;;                (let ((association-a (stream-car stream-a))
;;                      (association-b (stream-car stream-b)))
;;                  (let ((comparison
;;                         (compare-keys (car asosciation-a)
;;                                       (car association-b))))
;;                    (case comparison
;;                      ((0)
;;                       (and (datum=? (cdr association-a) (cdr association-b))
;;                            (loop (stream-cdr stream-a)
;;                                  (stream-cdr stream-b))))
;;                      ((-1 +1) #f)
;;                      (else (error:not-comparison comparison))))))
;;           (not (stream-pair? stream-b))))))
;;
;; (define (bb-tree/set-equal? bb-tree-a bb-tree-b)
;;   (bb-tree/equal? bb-tree-a bb-tree-b (lambda (a b) a b #t)))
;;
;; ;;; Iterator for foof-loop; see
;; ;;; <http://mumble.net/~campbell/scheme/foof-loop.txt>.
;;
;; (define-syntax in-bb-tree
;;   (syntax-rules ()
;;     ((IN-BB-TREE (key-variable datum-variable)
;;                  (bb-tree-expression)
;;                  next . rest)
;;      (next (((BB-TREE) bb-tree-expression))          ;Outer bindings
;;            ((STREAM (BB-TREE->STREAM BB-TREE)        ;Loop variables
;;                     STREAM*))
;;            ()                                        ;Entry bindings
;;            ((NOT (STREAM-PAIR? STREAM)))             ;Termination conditions
;;            (((key-variable datum-variable)           ;Body bindings
;;              (LET ((ASSOCIATION (STREAM-CAR STREAM)))
;;                (VALUES (CAR ASSOCIATION) (CDR ASSOCIATION))))
;;             ((STREAM*) (STREAM-CDR STREAM)))
;;            ()                                        ;Final bindings
;;            next . rest))))

;;;; Miscellaneous BB-Tree Types

;;; Why order predicates rather than comparators here?  Scheme comes
;;; with built-in order predicates but no built-in comparators.

(define (symbol<? a b)
  (string<? (symbol->string a)
            (symbol->string b)))

(define bb-tree-type:real-number (make-bb-tree-type/order-predicate <))
(define bb-tree-type:exact-integer (make-bb-tree-type/order-predicate <))
(define bb-tree-type:symbol (make-bb-tree-type/order-predicate symbol<?))
(define bb-tree-type:string (make-bb-tree-type/order-predicate string<?))
(define bb-tree-type:string-ci (make-bb-tree-type/order-predicate string-ci<?))
