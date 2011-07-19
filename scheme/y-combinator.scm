;;;; The Y Combinator

;;; In this file we derive the Y combinator, one of the fundamental results of recursive procedure theory. You already know that in some cases it is not necessary to give a procedure a name. For example,

  ((lambda (x) (+ x 1)) 6)

;; adds 1 to 6 without naming the procedure that does it. But, what about a recursive procedure? For example,

  (define fact
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (- n 1))))))

;; which computes the factorial of a number n, seems to need the name "fact" so that in the last line of the procedure it can recurse on itself. But, we will see this is not necessary and, in the process, will develop a lot of intuition about using Scheme. We proceed step by step, changing "fact" slightly on each step.

;; Step 1. The first idea is simply to pass "fact" in as an argument in much the same way that we did for

  (define op-maker
    (lambda (op)
      (lambda (x y)
        (op x y))))

;; The first lambda passes the name of the operation and the second lambda is the nameless operation. Let's try this with "fact". The first attempt is

  (define fact-maker
    (lambda (procedure)
      (lambda (n)
        (if (zero? n)
            1
            (* n (procedure (- n 1)))))))

;; The idea will be to pass "fact-maker" in through "procedure". Thus, what we would like to do is invoke (fact-maker fact-maker) to produce our nameless (well, almost nameless) factorial procedure. This would allow us to write, for example

  ((fact-maker fact-maker) 5)
  120

;; But, this doesn't work because "fact-maker" is a procedure which takes as input one argument that is a procedure but "procedure", which is supposed to be identical to "fact", requires a numeric argument. The solution is the following:

  (define fact-maker
    (lambda (procedure)
      (lambda (n)
         (if (zero? n)
             1
             (* n ((procedure procedure) (- n 1)))))))

;; Try this, for example, with

 >((fact-maker fact-maker) 5)

;; Well, we got the name out of the body of the procedure but we still have to pass the procedure in and so far we have been using a name to do that. So let's try to get the whole dependence on a name out.

;; Step 2. Recall we demand that "fact" be identical to (procedure procedure) which in turn must be identical to (fact-maker fact-maker) (recall the example ((fact-maker fact-maker) 5) which gives the same result as (fact 5)). Thus, we can write "fact-maker" in the following way, making use of the result of step 1.

  (define fact
    ((lambda (procedure)
       (lambda (n)
         (if (zero? n)
             1
             (* n ((procedure procedure) (- n 1))))))
     (lambda (procedure)
       (lambda (n)
         (if (zero? n)
             1
             (* n ((procedure procedure) (- n 1))))))))

;; Try this with >(fact 5)

;; Consider the following:

  (((lambda (procedure)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((procedure procedure) (- n 1))))))
    (lambda (procedure)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((procedure procedure) (- n 1)))))))
   5)

;; This produces the factorial of 5 because the procedure which is invoked (the huge mess) is exactly the definition of "fact." But, lo and behold, there is no name for this procedure anywhere!

;; In what follows, we try to generalize this to all procedures and wind up with the dreaded applicative-order Y-combinator.

;; Step 3. First, we need to separate out the part that pertains to computing the factorial. The goal is to write this part in one place and when code for other problems is substituted for the factorial code, the result will be a new recursive procedure. This step is a little tricky because we insist on using, with no significant changes, code that was designed assuming a procedure name. The section of factorial code we currently have, from step 2, is

  (define F
    (lambda (n)
      (if (zero? n)
          1
          (* n ((procedure procedure) (- n 1))))))

;; This is different from what we want because it contains a (procedure procedure) where we would like to see a plain old procedure. So, we use a trick to get it out. In general, isn't

  (f arg)

;; identical to

  ((lambda (x) (f x)) arg) ?

;; The second statement is a little strange, though, because it makes you pass "arg" into a procedure so that the procedure which would be applied to it anyway is applied. Why do we want to do such a thing? Watch! This means that

  ((procedure procedure) (- n 1))

;; is the same as

  ((lambda (arg) ((procedure procedure) arg)) (- n 1))

;; and we substitute this into our current version of F to get

  (define F
    (lambda (n)
      (if (zero? n)
          1
          (* n ((lambda (arg) ((procedure procedure) arg)) (- n 1))))))

;; How has this helped? Well, the (lambda (arg)...) is ONE procedure and procedures can be passed as arguments so F can be defined as

  (define F
    ((lambda (func-arg)
       (lambda (n)
         (if (zero? n)
             1
             (* n (func-arg (- n 1))))))
     (lambda (arg) ((procedure procedure) arg))))

;; Yes, it's the same F but the old definition looked like this:

  (define F (lambda (n) ... < procedure >))

;; and the new definition looks like this:

  (define F ((lambda (func-arg) (lambda (n) ...)) < procedure >))

;; where < procedure > is the (lambda (arg) ((procedure... ) ...) ...) expression

;; Step 4. - Now we are ready to take the result of step 3 and apply it to the result of step 2. Writing out the whole thing, we get:

  (define fact
    ((lambda (procedure)
       ((lambda (func-arg)
          (lambda (n)
            (if (zero? n)
                1
                (* n (func-arg (- n 1))))))
        (lambda (arg) ((procedure procedure) arg))))
     (lambda (procedure)
       ((lambda (func-arg)
          (lambda (n)
            (if (zero? n)
                1
                (* n (func-arg (- n 1))))))
        (lambda (arg) ((procedure procedure) arg))))))

;; You will probably want to study this carefully. Notice the double left parens in front of ((lambda (func-arg)... This is because we are writing

   ...
   ((lambda (func-arg) < body-using-func-arg >) (lambda (arg) ...))

;; which has the same form as

  ((lambda (arg) ((procedure procedure) arg)) (- n 1))

;; but is different in that a procedure is passed as an "arg" instead of an integer.

;; The two expressions beginning with (lambda (func-arg) ...) are exactly the pieces of code that correspond to the factorial code and they are in exactly the right form. So we can get them out of the definition of fact in the following way:

  (define F*
    (lambda (func-arg)
      (lambda (n)
        (if (zero? n)
            1
            (* n (func-arg (- n 1)))))))

  (define fact
    ((lambda (procedure)
       (F* (lambda (arg) ((procedure procedure) arg))))
     (lambda (procedure)
       (F* (lambda (arg) ((procedure procedure) arg))))))

;; This is significant because we can now use any procedure in place of F* to change functionality to whatever we want. The only problem is that, as written, we still need to name F*. This is easily remedied in the next step.

;; Step 5. Jackpot! Now we write the dreaded applicative-order Y-combinator:

  (define Y
    (lambda (X)
      ((lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg))))
       (lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg)))))))

;; Notice that the procedure which does our computation is X (we stopped using F* to emphasize this code can be applied to any procedure) and that is passed in as an argument.

;; Step 6. We can write "fact" in terms of the Y-combinator as follows:

  (define fact (Y F*))

;; Try >(fact 5) to check the result. For that matter, try >((Y F*) 5). But Y is general and F* is specific to factorial but with no name! If we wrote the whole thing out it would be

  (((lambda (X)
      ((lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg))))
       (lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg))))))
    (lambda (func-arg)
      (lambda (n)
        (if (zero? n)
            1
            (* n (func-arg (- n 1)))))))
   5)

;; Look Ma! No name! Just to show the generality of all this let's use the Y combinator to define another procedure. Say findmax - finding the largest integer in a list.

  (define findmax
    (lambda (l)
      (if (null? l)
          'no-list
          (if (null? (cdr l))
              (car l)
              (max (car l) (findmax (cdr l)))))))

;; First, create the analog of F* for fact, call it M for max.

  (define M
    (lambda (func-arg)
      (lambda (l)
        (if (null? l)
            'no-list
            (if (null? (cdr l))
                (car l)
                (max (car l) (func-arg (cdr l))))))))

;; Now try ((Y M) '(4 5 6 3 4 8 6 2)) to see if it works. If you want to build it out it looks like this:

  (((lambda (X)
      ((lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg))))
       (lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg))))))
    (lambda (func-arg)
      (lambda (l)
        (if (null? l)
            'no-list
            (if (null? (cdr l))
                (car l)
                (max (car l) (func-arg (cdr l))))))))
   '(4 5 6 3 4 8 6 2))

;; As an assignment for the interested student, write findamx without using the procedure name "max". Just how many of the remaining names in findmax do you think can be disposed of? Talk about a nameless society... 
