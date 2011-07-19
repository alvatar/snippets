;;;; stack-combinators.scm
;;;; Kon Lovett, Mar '09
;;;; Portions from a 'comp.lang.scheme' posting by "wayo.cavazos@gmail.com"

(declare
  (usual-integrations)
  (generic)
  (inline)
  (local)
  (no-procedure-checks) )

(module stack-combinators (;export
  uni uni2 uni3 #;uni@
  bi bi2 bi3 bi@
  tri tri2 tri3 tri@
  dip
  dup dupd
  swap
  drop drop/2)
  
(import scheme chicken)

;;

(define uni
  (case-lambda
    ((x f c)  (c (f x)))
    ((f c)    (lambda (x) (uni x f c)))
    ((c)      (lambda (f) (uni f c)))
    (()       (lambda (c) (uni c)))))

(define uni2
  (case-lambda
    ((x y f c)  (c (f x y)))
    ((f c)      (lambda (x y) (uni2 x y f c)))
    ((c)        (lambda (f) (uni2 f c)))
    (()         (lambda (c) (uni2 c)))))

(define uni3
  (case-lambda
    ((x y z f c)  (c (f x y z)))
    ((f c)        (lambda (x y z) (uni3 x y z f c)))
    ((c)          (lambda (f) (uni3 f c)))
    (()           (lambda (c) (uni3 c)))))

#; ;UNUSED
(define uni@
  (case-lambda
    ((x f c)  (c (f x)))
    ((f c)    (lambda (x) (uni@ x f c)))))

;;

(define bi
  (case-lambda
    ((x f g c)  (c (f x) (g x)))
    ((f g c)    (lambda (x) (bi x f g c)))
    ((f g)      (lambda (c) (bi f g c)))
    ((c)        (lambda (f g) (bi f g c)))
    (()         (lambda (c) (bi c)))))

(define bi2
  (case-lambda
    ((x y f g c)  (c (f x y) (g x y)))
    ((f g c)      (lambda (x y) (bi2 x y f g c)))
    ((f g)        (lambda (c) (bi2 f g c)))
    ((c)          (lambda (f g) (bi2 f g c)))
    (()           (lambda (c) (bi2 c)))))

(define bi3
  (case-lambda
    ((x y z f g c)  (c (f x y z) (g x y z)))
    ((f g c)        (lambda (x y z) (bi3 x y z f g c)))
    ((f g)          (lambda (c) (bi3 f g c)))
    ((c)            (lambda (f g) (bi3 f g c)))
    (()             (lambda (c) (bi3 c)))))

(define bi@
  (case-lambda
    ((x y f c)  (c (f x) (f y)))
    ((f c)      (lambda (x y) (bi@ x y f c)))))

;;

(define tri
  (case-lambda
    ((x f g h c)  (c (f x) (g x) (h x)))
    ((f g h c)    (lambda (x) (tri x f g h c)))
    ((f g h)      (lambda (c) (tri f g h c)))
    ((c)          (lambda (f g h) (tri f g h c)))
    (()           (lambda (c) (tri c)))))

(define tri2
  (case-lambda
    ((x y f g h c)  (c (f x y) (g x y) (h x y)))
    ((f g h c)      (lambda (x y) (tri2 x y f g h c)))
    ((f g h)        (lambda (c) (tri2 f g h c)))
    ((c)            (lambda (f g h) (tri2 f g h c)))
    (()             (lambda (c) (tri2 c)))))

(define tri3
  (case-lambda
    ((x y z f g h c)  (c (f x y z) (g x y z) (h x y z)))
    ((f g h c)        (lambda (x y z) (tri3 x y z f g h c)))
    ((f g h)          (lambda (c) (tri3 f g h c)))
    ((c)              (lambda (f g h) (tri3 f g h c)))
    (()               (lambda (c) (tri3 c)))))

(define tri@
  (case-lambda
    ((x y z f g h c)  (c (f x) (g y) (h z)))
    ((f g h c)        (lambda (x y z) (tri@ x y z f g h c)))))

;;

(define dip
  (case-lambda
    ((x y f c)   (c (f x) y))
    ((f c)       (lambda (x y) (dip x y f c)))))

;;

(define dup
  (case-lambda
    ((x c)  (c x x))
    ((c)    (lambda (x) (dup x c)))))

(define dupd
  (case-lambda
    ((x y c)  (c x x y))
    ((c)      (lambda (x y) (dupd x y c)))))

;;

(define swap
  (case-lambda
    ((x y c)  (c y x))
    ((c)      (lambda (x y) (swap x y c)))))

;;

(define drop
  (case-lambda
    ((x c)  (c))
    ((c)    (lambda (x) (drop x c)))))

(define drop/2
  (case-lambda
    ((x y c)  (c x))
    ((c)      (lambda (x y) (drop/2 x y c)))))

) ;module stack-combinators
