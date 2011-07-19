P mentioned (in a comment to my answer) that his implementation of Scheme does not have call-with-values. Here's a way to fake it (if you can ensure that the <values> symbol is never otherwise used in your program: you can replace it with (void), (if #f #f), or whatever you like that's not used, and that's supported by your implementation):
(define (values . items)
  (cons '<values> items))

(define (call-with-values source sink)
  (let ((val (source)))
    (if (and (pair? val) (eq? (car val) '<values>))
        (apply sink (cdr val))
      (sink val))))

What this does is that it fakes a multi-value object with a list that's headed by the <values> symbol. At the call-with-values site, it checks to see if this symbol is there, and if not, it treats it as a single value.

If the leftmost function in your chain can possibly return a multi-value, your calling code has to be prepared to unpack the <values>-headed list. (Of course, if your implementation doesn't have multiple values, this probably won't be of much concern to you.)

