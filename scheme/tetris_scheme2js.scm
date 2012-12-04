;; Tetris with Scheme2js - 2006/09/02
;; Copyright (c) 2006 Teruaki Gemma
;; Licensed under the GPL

;;Constants
(define width 10)
(define height 18)
;;Starting line for each block.
(define data-blk-start-at (vector 0 -1 -1 -1 0 0 -1))
;;Points per number of lines.
(define data-points (vector 0 40 100 300 1200))
;;Constants end

(define data-blk-colors '())
(define data-blk-shapes '())
(define grid '())
(define blk-y 0)
(define blk-x 0)
(define blk-r 0)
(define blk-shape 0)
(define next-blk-shape 0)
(define lines 0)
(define level 0)
(define score 0)
(define duration 600)

(define timer 0)

;; Initialize only once 
(define (init-game)
  (init-data-blk-shapes)
  (init-data-blk-colors))

;; Initialize to start the game
(define (start-game)
  (set! lines 0)
  (set! level 0)
  (set! score 0)
  (print-stats)
  (set! grid (map (lambda (l) (make-list width 0)) (make-list height)))
  (set! next-blk-shape (new-blk-shape))
  (shift)
  (update)
  (Event.observe window.document "keydown" (lambda (e) (key-event e)) #f)
  (set! timer (window.setInterval (lambda () (move 1 0 0)) duration)))

(define (update)
  (define touch-down-flag #f)
  ;;Clear
  (for-each-yx (lambda (y x)
		 (set-html-grid-color! y x (cell-ref y x)))
	       height width)
  ;;Draw & Check the touch-down
  (for-each-blk-cell (lambda (y x)
		       (let ((cy (+ blk-y y))
			     (cx (+ blk-x x)))
			 (if (valid-coord? cy cx)
			     (set-html-grid-color! cy cx (+ blk-shape 1)))
			 (if (touch-down? cy cx)
			     (set! touch-down-flag #t))))
		     blk-shape blk-r)
  (if touch-down-flag
      (touch-down)))

(define (touch-down)
  ;;Fix the block
  (for-each-blk-cell (lambda (y x)
		       (let ((cy (+ blk-y y))
			     (cx (+ blk-x x)))
			 (cell-set! cy cx (+ blk-shape 1))))
		     blk-shape blk-r)

  ;;Remove complete line
  (set! grid (remove! complete-line? grid))
  (let ((complete-line-count (- height (length grid))))
    ;;Supply brand-new lines on the head.
    (let loop ((c complete-line-count))
      (if (> c 0)
	  (begin
	    (set! grid (cons (make-list width 0) grid))
	    (loop (- c 1)))))
    (update-stats complete-line-count))

  (if (or (cell-overlay? 0 4)
	  (cell-overlay? 0 5))
      ;;Gameover.
      (begin
	(window.clearInterval timer)
	(for-each-yx (lambda (y x)
		       (if (cell-overlay? y x)
			   (set-html-grid-color! y x 1)))
		     height width))
      ;; Else
      (shift)))

(define (update-stats complete-line-count)
  (if (> complete-line-count 0)
      (begin
	(set! lines (+ lines complete-line-count))
	(set! level (quotient lines 10))
	(set! score (+ score (* (+ level 1) (vector-ref data-points complete-line-count))))
	(print-stats)
	;;Level up
	(set! duration (- 600 (* level 10)))
	(window.clearInterval timer)
	(set! timer (window.setInterval (lambda () (move 1 0 0)) duration)))))

;;The next shape becomes the current one; reset coordinates
(define (shift)
  (set! blk-x 3)
  (set! blk-r 0)
  (set! blk-shape next-blk-shape)
  (set! blk-y (vector-ref data-blk-start-at blk-shape))
  (set! next-blk-shape (new-blk-shape))
  (update-next-blk-grid))

(define (update-next-blk-grid)
  ;;Clear
  (for-each-yx (lambda (y x)
		 (set-html-next-blk-grid-color! y x 0))
	       4 4)
  ;;Draw
  (for-each-blk-cell (lambda (y x)
		       (set-html-next-blk-grid-color! y x (+ next-blk-shape 1)))
		     next-blk-shape 0))

(define (move dy dx dr)
  (let ((can-go #t)
	(blk-new-y (+ blk-y dy))
	(blk-new-x (+ blk-x dx))
	(blk-new-r (modulo (+ blk-r dr) 4)))
    (for-each-blk-cell (lambda (y x)
			 (let ((cy (+ blk-new-y y))
			       (cx (+ blk-new-x x)))
			   (if (or (not (valid-coord? cy cx))
				   (cell-overlay? cy cx))
			       (set! can-go #f))))
		       blk-shape blk-new-r)
    (if can-go
	(begin
	  (set! blk-y blk-new-y)
	  (set! blk-x blk-new-x)
	  (set! blk-r blk-new-r))))
  (update))

(define (key-event e)
  (let ((key-code e.keyCode))
    (case key-code
      ;;Left
      ((37 74 106 68) (move 0 -1 0))
      ;;Right
      ((39 76 108 78) (move 0 1 0))
      ;;Rotate
      ((38 73 105 84) (move 0 0 1))
      ;;Down
      ((40 75 107 72) (move 1 0 0)))))
    
(define (touch-down? y x)
  (let ((cy (+ y 1)))
    (or 
     (>= cy height)
     (cell-overlay? cy x))))

(define (complete-line? line)
  (every (lambda (c) (> c 0)) line))

(define (valid-coord? y x)
  (and
   (>= y 0)
   (< y height)
   (>= x 0)
   (< x width)))

(define (cell-set! y x val)
  (set-car! (list-tail (list-ref grid y) x) val))

(define (cell-ref y x)
  (list-ref (list-ref grid y) x))

(define (cell-overlay? y x)
  (> (cell-ref y x) 0))

(define (set-html-grid-color! y x col)
  (let ((el (document.getElementById (+ "grid-" y "-" x))))
    (set! el.style.background (vector-ref data-blk-colors col))))

(define (set-html-next-blk-grid-color! y x col)
  (let ((el (document.getElementById (+ "next" y "-" x))))
    (set! el.style.background (vector-ref data-blk-colors col))))

(define (print-stats)
  ;;Embededd (for-each proc lis1 lis2) in hop-1.4.0 seems not to work.
  ;;(for-each (lambda (id x)
  ;; 	      (let ((el (document.getElementById id)))
  ;; 		(set! el.innerHTML x)))
  ;;   	    (list "lines" "level" "score")
  ;; 	    (list lines level score)))
  (define (p id x)
    (let ((el (document.getElementById id)))
      (set! el.innerHTML x)))
  (p "lines" lines)
  (p "level" level)
  (p "score" score))

(define (for-each-blk-cell fun blk-shape blk-r) 
  (let ((tet-s (data-blk-shapes-ref blk-shape blk-r)))
    (let loop0 ((y 0))
      (if (< y 4)
	  (begin
	    (let loop1 ((x 0))
	      (if (< x 4)
		  (begin
		    (if (= (list-ref tet-s (+ (* y 4) x)) 1)
			(fun y x))
		    (loop1 (+ x 1)))))
	  (loop0 (+ y 1)))))))

(define (for-each-yx fun h w) 
  (let loop0 ((y 0))
    (if (< y h)
	(begin
	  (let loop1 ((x 0))
	    (if (< x w)
		(begin
		  (fun y x)
		  (loop1 (+ x 1)))))
	  (loop0 (+ y 1))))))

(define (new-blk-shape)
  (define (random-int n)
    (Math.floor (* (Math.random) n)))
  (random-int 7))

(define (data-blk-shapes-ref blk-shape blk-r)
  (list-ref (list-ref data-blk-shapes blk-shape) blk-r))

(define (init-data-blk-shapes)
  (define (rotate-90-clockwise blk)
    (let ((rot-table (list 12 8  4 0
			   13 9  5 1
			   14 10 6 2
			   15 11 7 3)))
      (map (lambda (c)
	     (list-ref blk c))
	   rot-table)))
  (define (rotate blk)
    (let* ((r0 blk)
	   (r1 (rotate-90-clockwise r0))
	   (r2 (rotate-90-clockwise r1))
	   (r3 (rotate-90-clockwise r2)))
      (list r0 r1 r2 r3)))
  (set! data-blk-shapes (map rotate 
			     (list
			      ;;I
			      (list
			       0 1 0 0
			       0 1 0 0
			       0 1 0 0
			       0 1 0 0)
			      ;;O
			      (list
			       0 0 0 0
			       0 1 1 0
			       0 1 1 0
			       0 0 0 0)
			      ;;S
			      (list
			       0 0 0 0
			       0 0 1 1
			       0 1 1 0
			       0 0 0 0)
			      ;;Z
			      (list
			       0 0 0 0
			       1 1 0 0
			       0 1 1 0
			       0 0 0 0)
			      ;;J
			      (list
			       0 0 1 0
			       0 0 1 0
			       0 1 1 0
			       0 0 0 0)
			      ;;L
			      (list
			       0 1 0 0
			       0 1 0 0
			       0 1 1 0
			       0 0 0 0)
			      ;;T
			      (list
			       0 0 0 0
			       1 1 1 0
			       0 1 0 0
			       0 0 0 0)))))

(define (init-data-blk-colors)
  (set! data-blk-colors (vector "#ffffff"
				;;I
				"#00ffff"
				;;O
				"#f0f000"
				;;S
				"#00ff00"
				;;Z
				"#ff0000"
				;;L
				"#0000ff"
				;;J
				"#ff800f"
				;;T
				"#ff00ff")))

;; a cheap implementation of SRFI-1 "every"
(define (every pred lis)
  (let lp ((head (car lis)) (tail (cdr lis)))
    (if (null? tail)
	(pred head)
	(and (pred head) (lp (car tail) (cdr tail))))))

;; SRFI-1 - List processing library - "filter!" & "remove!"

(define (remove! pred l) (filter! (lambda (x) (not (pred x))) l))

;; This "filter!" code is based on the reference implementation by Olin Shivers

;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

(define (filter! pred lis)
  (let lp ((ans lis))
    (cond ((null? ans) ans)			; Scan looking for
	  ((not (pred (car ans))) (lp (cdr ans)))	; first cons of result.

	  ;; ANS is the eventual answer.
	  ;; SCAN-IN: (CDR PREV) = LIS and (CAR PREV) satisfies PRED.
	  ;;          Scan over a contiguous segment of the list that
	  ;;          satisfies PRED.
	  ;; SCAN-OUT: (CAR PREV) satisfies PRED. Scan over a contiguous
	  ;;           segment of the list that *doesn't* satisfy PRED.
	  ;;           When the segment ends, patch in a link from PREV
	  ;;           to the start of the next good segment, and jump to
	  ;;           SCAN-IN.
	  (else (letrec ((scan-in (lambda (prev lis)
				    (if (pair? lis)
                                      (if (pred (car lis))
                                        (scan-in lis (cdr lis))
                                        (scan-out prev (cdr lis))))))
			 (scan-out (lambda (prev lis)
				     (let lp ((lis lis))
				       (if (pair? lis)
                                         (if (pred (car lis))
                                           (begin (set-cdr! prev lis)
                                                  (scan-in lis (cdr lis)))
                                           (lp (cdr lis)))
                                         (set-cdr! prev lis))))))
		  (scan-in ans (cdr ans))
		  ans)))))
