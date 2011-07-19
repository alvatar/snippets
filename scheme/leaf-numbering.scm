(define-syntax let-values
  (syntax-rules ()
    ((let-values (?binding ...) ?body0 ?body1 ...)
     (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
    
    ((let-values "bind" () ?tmps ?body)
     (let ?tmps ?body))
    
    ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
     (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
    
    ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
     (call-with-values 
       (lambda () ?e0)
       (lambda ?args
         (let-values "bind" ?bindings ?tmps ?body))))
    
    ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
    
    ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (call-with-values
       (lambda () ?e0)
       (lambda (?arg ... . x)
         (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))

(define state 0) 

#;(define (number-tree tree) 
  (if (pair? tree) 
    (let* ((left-subtree (number-tree (car tree))) 
           (right-subtree (number-tree (cdr tree)))) 
      (cons left-subtree right-subtree)) 
    (let ((n state)) 
      (set! state (+ state 1)) 
      n)))

(define (number-tree tree state) 
  (if (pair? tree) 
    (let-values (((lft state-1) (number-tree (car tree) state)))
                ;(display "1*************\n")
                ;(pp state-1) (pp lft)
      (let-values (((rgt state-2) (number-tree (cdr tree) state-1)))
                  ;(display "2*************\n")
                  ;(pp state-2) (pp rgt)
        (values (cons lft rgt) state-2))) 
    (values (+ state 1) (+ state 1))))

;(let-values (((a b) (number-tree '(l l ((l) l l) l l l (l (((l) l) l))) 0))) (pp a))
(let-values (((a b) (number-tree '(l l ((l) l)) 0))) (pp a))
