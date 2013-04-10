;; Inspired by
;; http://matt.might.net/articles/higher-order-list-operations/
;; (c) 2013 Álvaro Castro-Castilla
(##import-include core: match-macros)
(##import fabric: algorithm/comprehension)

(define double (lambda (x) (* 2 x)))
(define numbers (list-ec (:range x 0 10) x))

(pp numbers)

;; Map with matching
(define (map/match f lst)
  (match
   lst
   ('() '())
   ((hd . tl)(cons (f hd) (map/match f tl)))))

(pp (map/match double numbers))

;; Map with comprehensions
(define (map/comprehensions f lst)
  (list-ec (: i lst) (f i)))

(pp (map/comprehensions double numbers))

;; Filter with matching
(define (filter/match p? lst)
  (match
   lst
   ('() '())
   (((? p?) . tl) (cons (car lst) (filter/match p? tl)))
   ((hd . tl) (filter/match p? tl))))

(pp (filter/match even? numbers))

;; Filter with comprehensions
(define (filter/comprehensions p? lst)
  (list-ec (: i lst) (if (p? i)) i))

(pp (filter/comprehensions even? numbers))

;; Folding with comprehesions
(define (fold/comprehensions f lst)
  (fold-ec 0 (: i lst) i f))

(pp (fold/comprehensions + numbers))

;; Reduce with matching
(define (reduce/match op lst)
  (match
   lst
   ('() (error "no elements in list"))
   ((a) a)
   ((hd . tl) (op hd (reduce/match op tl)))))

(pp (reduce/match + numbers))

;; Zipping with match
(define (zip/match lst1 lst2)
  (match
   (list lst1 lst2)
   (('() '()) '())
   (((hd1 . tl1) (hd2 . tl2))
    (cons (list hd1 hd2) 
          (zip/match tl1 tl2)))))

(pp (zip/match (list-ec (:range i 1 5) i)
               (list-ec (:range i 4 8) i)))

;; Zipping with comprehensions
(define (zip/comprehensions lst1 lst2)
  (list-ec (:parallel
            (: i lst1)
            (: j lst2))
           (list i j)))

(pp (zip/comprehensions (list-ec (:range i 1 5) i)
                        (list-ec (:range i 4 8) i)))

;; Unzip with match
(define (unzip/callback lst k)
  (match
   lst
   ('() (k '() '()))
   (((a b) . tl)
    (unzip/callback tl (lambda (as bs)
                         (k (cons a as) (cons b bs)))))))

(pp (unzip/callback '((1 2) (3 4) (5 6))
                    (lambda (as bs) as)))

;; Partition with match
(define (partition/values p? lst)
  (match
   lst
   ('()
    (values '() '()))
   ((hd . tl)
    (receive (ins outs)
             (partition/values p? tl)
             (if (p? hd)
                 (values (cons hd ins) outs)
                 (values ins (cons hd outs)))))))

(receive (a b)
         (partition/values even? numbers)
         (pp a) (pp b))
