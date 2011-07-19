;;;; let-objects.scm -- let-objects syntax

;;; Created:    <2002-12-28 01:00:31 foof>
;;; Time-stamp: <2003-01-09 15:23:44 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

(define-module util.let-objects
  (use srfi-1)
  (use srfi-2)
  (use srfi-11)
  (use gauche.parameter)
  (export object-rules let-objects make-object
          with-object-rules define-object-rules
          object-property-get object-property-set!
          object-cpl object-isa? object-class-get-slot
          object-class-get-multi get-key get-key-ls))
(select-module util.let-objects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default object rules, with default initialization rules for all
;; classes

(define object-rules
  (make-parameter
   `((object ,<object> ()
      (init
       ,(lambda (obj parent keys)
          ;; handle simple initializers based on keywords
          (for-each
           (lambda (k)
             (let loop ((args (cddr k)) (vals '()))
               (cond ((null? args)
                      (apply (car k) obj (reverse vals)))
                     ((pair? args)
                      (and-let* ((val (get-key-ls (car args) keys)))
                        (loop (cdr args) (cons (car val) vals))))
                     (else
                      (apply (car k) obj (append (reverse vals) args))))))
           (object-class-get-multi obj 'keywords) )))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

;; two differences between this and get-keyword:
;;   1) get-key-ls is less strict (doesn't require a proper keyword list)
;;   2) get-key-ls automatically forces the result
(define (get-key-ls key ls)
  (cond ((null? ls)
         #f)
        ((eq? key (car ls))
         (cons (force (cadr ls)) (cddr ls)))
        ((keyword? (car ls))
         (get-key-ls key (cddr ls)))
        (else
         (get-key-ls key (cdr ls)))))

(define (get-key key ls . default)
  (let1 a (get-key-ls key ls)
    (if a (car a) (if (pair? default) (car default) #f))))

(define (object-get-rules type)
  (let ((rules (object-rules)))
    (cond ((symbol? type)
           (assq type rules))
          ((is-a? type <class>)
           (find (lambda (r) (eq? type (cadr r))) rules))
          (else
           (let ((cl (class-of type)))
             (find (lambda (r) (eq? cl (cadr r))) rules))))))

(define (object-cpl class)
  (and-let* ((rules (object-get-rules class))
             (c (car rules)))
    (cons c (apply append (map object-cpl (caddr (or (assq c (object-rules))
                                                     '(1 2 ()))))))))

(define (object-get-class-field class field . default)
  ;;(format #t "(get-class-field ~S ~S)\n" class field)
  (or (and-let* ((rules (object-get-rules class))
                 (rule (assq field rules)))
        (cdr rule))
      (if (pair? default) (car default) #f)))

(define (object-class-get-slot class prop)
  (let loop ((ls (object-cpl class)))
    (or (object-get-class-field (car ls) prop)
        (and (pair? ls) (loop (cdr ls))))))

(define (object-class-get-multi class prop)
  (apply append (map (cut object-get-class-field <> prop '())
                     (object-cpl class))))

(define (object-isa? obj class)
  (memq class (object-cpl obj)))

(define (construct-object type keys)
  (or (and-let*
          ((maker (object-class-get-slot type 'make))
           (proc (car maker))
           (args (cdr maker))
           (vals (map (lambda (arg)
                        (let* ((sym (car arg))
                               (key (make-keyword sym)))
                          (get-key key keys
                                      (get-key :default (cdr arg)))))
                      (if (pair? args) (car args) '())))
           )
        (apply proc vals))
      (error "unknown object type: " type)))

(define (initialize-object obj parent keys)
  (and-let* ((child-rules (object-get-rules obj)))
    ;; run automatic initializers
    (let ((inits (object-class-get-multi (car child-rules) 'init)))
      (for-each (cut <> obj parent keys) inits))
    ))

(define (post-init-object obj parent keys key-hash kids)
  ;;(format #t "(post-init-object ~S ~S ~S ~S)\n" obj parent kids keys)
  (and-let* ((child-rules (object-get-rules obj)))
    ;; run post-initialization procedures
    (for-each
     (cut <> obj parent keys key-hash kids)
     (or (object-class-get-multi (car child-rules) 'post-init) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface

(define-macro (let-objects tree . body)
  (define (kids ls)
    (cond ((null? ls)
           '())
          ((keyword? (car ls))
           (kids (cddr ls)))
          ((pair? (car ls))
           (cons (car ls) (kids (cdr ls))))
          (else
           (kids (cdr ls)))))
  (define (keys ls)
    (cond ((null? ls)
           '())
          ((keyword? (car ls))
           (if (eq? (car ls) :value)
             (keys (cddr ls))
             ;;(cons (car ls) (cons (cadr ls) (keys (cddr ls))))
             `(,(car ls) (delay ,(cadr ls)) . ,(keys (cddr ls)))
             ))
          (else (keys (cdr ls)))))
  (let ((decls '())
        (keydefs '())
        (makes '())
        (post-makes '())
        (inits '())
        (post-inits '())
        (key-hash (make-hash-table 'eq?))
        (key-hash-name (gensym)))
    (let loop ((tr tree) (parent #f))
      (let* ((type (unwrap-syntax (car tr)))
             (rest (cdr tr))
             (name (and (pair? rest) (car rest)))
             (keyname (gensym)))
        ;; use a gensym if no name given
        (if (or (symbol? name) (identifier? name))
          (if (pair? rest)
            (set! rest (cdr rest)))
          (set! name (gensym)))
        ;;(format #t "type: ~S name: ~S keyname: ~S\n" type name keyname)
        ;; add to decls
        (push! decls name)
        ;; add to keydefs
        (push! keydefs (list keyname `(list ,@(keys rest))))
        ;; add to constructors, optionally using :value
        (push! makes
               `(set! ,name ,(get-key :value rest
                                      `(,construct-object ',type ,keyname))))
        (push! post-makes `(,hash-table-put! ,key-hash-name ,name ,keyname))
        ;; add to initializers
        (push! inits `(,initialize-object ,name ,parent ,keyname))
        ;; recurse on children
        (let* ((children (kids rest))
               (cur-count (length decls))
               (kid-count (length children)))
          (for-each
           (cut loop <> (if (object-isa? type 'container) name parent))
           children)
          ;; pull out the children's names from the declarations and add
          ;; the post-init step
          (push! post-inits `(,post-init-object
                              ,name ,parent ,keyname ,key-hash-name
                              (,list ,@(take (reverse (drop-right decls cur-count))
                                             kid-count))
                              )))))
    `(letrec (,@(map (cut list <> #f) (reverse decls))
              ,@(reverse keydefs)
              (,key-hash-name ,key-hash))
         ,@(reverse makes)
         ,@(reverse post-makes)
         ,@(reverse inits)
         ,@post-inits
         ,@body)))

(define (make-object ls)
  (and-let* ((type (car ls))
             (keys (cdr ls)))
    (let ((obj (cond ((get-key-ls :value keys) => car)
                     (else (construct-object type keys)))))
      (initialize-object obj #f keys)
      (post-init-object obj #f keys (make-hash-table 'eq?) '())
      obj)))

(define (object-property-get obj key)
  (let loop ((keys (obj-class-get-multi obj 'keywords)))
    ;;(format #t "keys: ~S\n" keys)
    (if (pair? keys)
      (let ((k (car keys)))
        ;; only handle single keywords
        (if (and (eq? key (caddr k)) (cadr k) (null? (cdddr k)))
          ((cadr k) obj)
          (loop (cdr keys))))
      ;; could maybe be done faster by examining the slots of obj
      ;; directly
      (or (and-let* ((props (object-class-get-multi obj 'properties))
                     (keyname (keyword->string key))
                     (p (assoc keyname props)))
            (and (memq 'read p)
                 (slot-ref obj (string->symbol keyname))))
          (error "object ~S has no such property: ~S\n" obj key)))))

(define (object-property-set! obj key val)
  (let loop ((keys (object-class-get-multi obj 'keywords)))
    (if (pair? keys)
      (let ((k (car keys)))
        ;; only handle single keywords
        (if (and (eq? key (caddr k)) (null? (cdddr k)))
          ((car k) obj val)
          (loop (cdr keys))))
      (or (and-let* ((props (object-class-get-multi obj 'properties))
                     (keyname (keyword->string key))
                     (p (assoc keyname props)))
            (and (memq 'write p)
                 (slot-set! obj (string->symbol key) val)))
          (error "object ~S has no such property: ~S\n" obj key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extending syntax

(define (merge-new-only orig new)
  new)

(define (merge-objects orig new)
  (let* ((classes (map car new))
         (keep (filter (lambda (c) (not (memq (car c) classes))) orig)))
    (append keep new)))

(define (merge-slots orig new)
  (error "feeling lazy"))

(define (merge-multi-slots orig new)
  (error "feeling lazy"))

(define (make-define-object-rules merger)
  (lambda (rules)
    (let ((orig-rules (object-rules)))
      (object-rules (merger orig-rules rules)))))

(define (make-with-object-rules merger)
  (lambda (rules body)
    (let* ((orig-rules (object-rules))
           (new-rules (merger orig-rules rules)))
      (dynamic-wind
          (lambda () (object-rules new-rules))
          body
          (lambda () (object-rules orig-rules))))))

;; use only the new definitions
(define define-objects (make-define-object-rules merge-new-only))
(define with-objects (make-with-object-rules merge-new-only))

;; merge objects, preferring the new definitions
(define define-object-rules (make-define-object-rules merge-objects))
(define with-object-rules (make-with-object-rules merge-objects))

;; merge objects, overlaying slots from new definitions
(define define-object-slots (make-define-object-rules merge-slots))
(define with-object-slots (make-with-object-rules merge-slots))

;; merge objects, overlaying multi-slots from new definitions
(define define-object-multi-slots (make-define-object-rules merge-multi-slots))
(define with-object-multi-slots (make-with-object-rules merge-multi-slots))

(provide "util/let-objects")
