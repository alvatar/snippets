 (define-syntax aif 
   (sc-macro-transformer 
    (lambda (form environment) 
      (let ((condition  
             (make-syntactic-closure environment '() (cadr form))) 
            (consequent  
             (make-syntactic-closure environment '(it) (caddr form))) 
            (alternative 
             (make-syntactic-closure environment '() (cadddr form)))) 
        `(let ((it ,condition)) 
           (if it
               ,consequent 
               ,alternative)))))) 


(pp
 (aif (assv 'a '((1 . 2) (a . b))) 
      (cdr it) 
      'nothing)
)
