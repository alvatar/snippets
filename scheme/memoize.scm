
(define-module util.memoize
  (use file.util)
  (use util.list)
  (use gauche.parameter)
  (export memoize memoize-to-file memoize-file-loader))
(select-module util.memoize)

(define (eq->hash-sym eq)
  (cond ((eq? eq eq?) 'eq?)
        ((eq? eq eqv?) 'eqv?)
        ((eq? eq equal?) 'equal?)
        ((eq? eq string=?) 'string=?)
        (else (error "unsupported hash equality:" eq))))

(define (make-memo-table eq)
  (make-parameter (make-hash-table (eq->hash-sym eq))))

(define (make-memoizer proc arity eq)
  (if (eqv? arity 0)
    ;; degenerate case, nothing to memoize
    proc
    (let ((h (make-memo-table eq)))
      (case arity ;; can expand other cases for efficiency
        ((1)
         (lambda (x)
           (unless (hash-table-exists? (h) x)
             (hash-table-put! (h) x (proc x)))
           (hash-table-get (h) x)))
        (else
         (lambda args
           (unless (hash-table-exists? (h) args)
             (hash-table-put! (h) args (apply proc args)))
           (hash-table-get (h) args)))))))

(define (memoize proc . opt-eq)
  (let ((eq (get-optional opt-eq equal?))
        (ar (arity proc)))
    (if (or (and (number? ar) (< ar 2))
            (eq? eq equal?))
      (make-memoizer proc ar eq)
      ;; autoload util.hash for this case?
      (error "unsupported memoization"))))

;; file may be either a literal string or a string transformer
(define (memoize-to-file proc . keys)
  (let-keywords* keys
      ((transform (lambda (f . args) (string-append f ".memo")))
       (validator (lambda args #t))
       (reader read)
       (writer write))
    (lambda args
      (let ((f (if (string? transform) transform (apply transform args))))
        (or (and (file-is-readable? f)
                 (apply validator f args)
                 (with-input-from-file f reader))
            (let ((res (apply proc args)))
              (when writer
                (with-output-to-file f (lambda () (writer res))))
              res))))))

(define *memo-file-magic* '(#x6D #x65 #x6D #x6F))

(define (read-memo-magic)
  (every (cut eqv? <> (read-byte)) *memo-file-magic*))

;; note this works fine for directories, and the procedure can have any
;; arity so long as the first argument is a file/directory name.
(define (memoize-file-loader proc . args)
  (let-keywords* args
      ((transform (lambda (f . args)
                    (string-append f (if (and (file-is-directory? f) (not (#/\/$/ f))) "/" "") ".memo")))
       (reader read)
       (writer write)
       (version -1.0))
    (memoize-to-file
     proc
     :transform transform
     :validator (lambda (cache orig . args)
                  (let ((mtime (file-mtime orig)))
                    (and (file-mtime>=? cache mtime)
                         (with-input-from-file cache
                           (lambda ()
                             (and (read-memo-magic)
                                  (>= (read) mtime)
                                  (equal? (read) version)))))))
     :reader (lambda () (read-memo-magic) (read) (read) (reader))
     :writer (if writer
               (lambda (res)
                 (for-each write-byte *memo-file-magic*) (newline)
                 (write (sys-time)) (newline)
                 (write version)    (newline)
                 (writer res))
               #f))))

(provide "util/memoize")
