;;
;; 
;; 1-dimensional interpolation routines.
;;
;; Copyright 2007-2009 Ivan Raikov and the Okinawa Institute of Science and Technology.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(module interp1d

 (interp1d:nearest interp1d:lbound interp1d:ubound interp1d:linear
  interp1d:piecewise-quadratic interp1d:from-sequence )

 (import scheme chicken foreign easyffi srfi-1 srfi-4 )


(define (interp1d:error x . rest)
  (let ((port (open-output-string)))
    (if (port? x)
	(begin
	  (display "[" port)
	  (display (port-name x) port)
	  (display "] " port)))
    (let loop ((objs (if (port? x) rest (cons x rest))))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'interp1d (get-output-string port)))
	  (begin (display (car objs) port)
		 (display " " port)
		 (loop (cdr objs)))))))


;; A sliding window over a sequence of data (list or stream)
(define (interp1d:from-sequence method step src . rest) 
  (let-optionals rest ((start 0) (data-car car) (data-cdr cdr) (data-null? null?) 
		       (delta-window-len 4) (max-window-len 128))
    (let ((tlo start) (thi start) 
	  (xdata (list start)) (ydata (list (data-car src))) 
	  (strm  (data-cdr src)))
      (lambda (t)
;	(print "t = " t)
;	(print "xdata = " xdata)
;	(print "ydata = " ydata)
	(cond ((= t tlo)  (exact->inexact (car ydata)))
	      ((= t thi)  (exact->inexact (last ydata)))
	      ((and (<= t thi) (<= tlo t))  (method xdata ydata t))
	      ((< thi t)
		 (let-values (((thi1 xs1 ys1 strm1)
			       (let loop ((delta  (fx+ delta-window-len (inexact->exact (floor (/ (- t thi) step)))))
					  (ti (+ step thi)) (xs (list)) (ys (list)) (strm strm))
				 (if (and (positive? delta))
				     (begin
				       (if (data-null? strm) 
					   (interp1d:error 'interp1d:from-sequence ": interpolation table is too short;"
						      "boundary exceeded at x = " ti))
				       (loop (fx- delta 1) (+ step ti) (cons ti xs) 
					     (cons (data-car strm) ys) (data-cdr strm)))
				     (values (- ti step) (reverse xs) (reverse ys) strm)))))
			     (set! ydata (append ydata ys1))
			     (set! xdata (append xdata xs1))
			     (set! thi   thi1)
			     (set! strm  strm1)
			     (let ((n (length ydata)))
			       (if (> n max-window-len)
				   (let ((delta (fx- n max-window-len)))
				     (set! tlo (+ tlo (* step delta)))
				     (set! ydata (drop ydata delta))
				     (set! xdata (drop xdata delta)))))
			     (method xdata ydata t)))
	      ((> tlo t)  (interp1d:error 'interp1d-from-sequence 
				     ": insufficient sliding window length for past data stream values: "
				     " tlo = " tlo " t = " t)))))))


(define (bounds x xi . rest)
  (let-optionals rest ((delta2 1))
   (let loop ((a 0) (b (fx- (length x) 1)))
     (let ((delta (fx- b a)))
       (if (positive? (fx- delta delta2))
	   (let ((midpoint (fx+ a (fx/ delta 2))))
	     (if (<= xi (list-ref x midpoint))
		 (loop a midpoint)
		 (loop midpoint b)))
	   (if (or (< xi (list-ref x a)) (< (list-ref x b) xi))
	       (values #f #f)
	       (values a b)))))))



(define (interp1d:lbound xp yp xi)
  (let ((n (length xp)))
    (if (not (positive? (- n 2))) (interp1d:error 'interp1d:lbound ": interpolation table is too short"))
    (let-values (((i1 i2)  (bounds xp xi)))
     (if (not (and i1 i2)) (interp1d:error 'interp1d:lbound ": value outside specified range"))
     (let ((x1 (list-ref xp i1))
	   (x2 (list-ref xp i2)))
       (let ((y1 (list-ref yp i1)))
	 y1)))))


(define (interp1d:ubound xp yp xi)
  (let ((n (length xp)))
    (if (not (positive? (- n 2))) (interp1d:error 'interp1d:ubound ": interpolation table is too short"))
    (let-values (((i1 i2)  (bounds xp xi)))
     (if (not (and i1 i2)) (interp1d:error 'interp1d:ubound ": value outside specified range"))
     (let ((x1 (list-ref xp i1))
	   (x2 (list-ref xp i2)))
       (let ((y2 (list-ref yp i2)))
	 y2)))))


(define (interp1d:nearest xp yp xi)
  (let ((n (length xp)))
    (if (not (positive? (- n 2))) (interp1d:error 'interp1d:nearest ": interpolation table is too short"))
    (let-values (((i1 i2)  (bounds xp xi)))
     (if (not (and i1 i2)) (interp1d:error 'interp1d:nearest ": value outside specified range"))
     (let ((x1 (list-ref xp i1))
	   (x2 (list-ref xp i2)))
       (let ((y1 (list-ref yp i1))
	     (y2 (list-ref yp i2)))
	 (if (< (abs (- xi x1)) (abs (- xi x2)))
	    y1 y2))))))



;;
;; Interpolate function y=f(x) at the point xi using the linear
;; interpolation method.
;;
(define (interp1d:linear xp yp xi)
  (let ((n (length xp)))
    (if (not (positive? (- n 2))) (interp1d:error 'interp1d:linear ": interpolation table is too short"))
;    (print "linear: xi = " xi)
;    (print "linear: n = " n)
    (let-values (((i1 i2)  (bounds xp xi)))
;      (print "linear: i1 = " i1)
;      (print "linear: i2 = " i2)
    (if (not (and i1 i2)) (interp1d:error 'interp1d:linear ": value outside specified range"))
    (let ((x1 (list-ref xp i1))
	  (x2 (list-ref xp i2)))
      (let ((y1 (list-ref yp i1))
	    (y2 (list-ref yp i2)))
	(let ((A  (/ (- y2 y1) (- x2 x1)))
	      (B  (- (/ (- (* x1 y2) (* y1 x2)) (- x2 x1)))))
	  (let ((yi (+ (* A xi) B)))
;	      (print "linear: x1 = " x1)
;	      (print "linear: x2 = " x2)
;	      (print "linear: y1 = " y1)
;	      (print "linear: y2 = " y2)
;	      (print "linear: p = " p)
;	      (print "linear: yi = " yi)
	    yi)))))))



; Parse & embed
#>!
  void pq_coeffs (double x1, double x2, double x3, 
		  double y1, double y2, double y3, double *ABC);
<#



;;
;; Interpolate function y=f(x) at the point xi using the piecewise
;; quadratic interpolation method.
;;
(define (interp1d:piecewise-quadratic xp yp xi)
  (let ((n (length xp)))
    (if (not (positive? (- n 3))) (interp1d:error 'piecewise-quadratic "interpolation table is too short"))
    (let-values (((i1 i2)  (bounds xp xi 2)))
;      (print "pq: i1 = " i1)
;      (print "pq: i2 = " i2)
      (if (not (and i1 i2)) (interp1d:error 'interp1d:piecewise-quadratic ": value outside specified range"))
      (let-values (((i1 i2 i3) (let ((i3 (+ 1 i2)))
				 (if (<= n i3)
				     (values (- i1 1) (- i2 1) i2)
				     (values i1 i2 i3)))))
;      (print "pq: i1 = " i1)
;      (print "pq: i2 = " i2)
;      (print "pq: i3 = " i3)
	(let ((x1 (list-ref xp i1))
	      (x2 (list-ref xp i2))
	      (x3 (list-ref xp i3)))
	  (let ((y1 (list-ref yp i1))
		(y2 (list-ref yp i2))
		(y3 (list-ref yp i3)))
	    (let-values (((A B C)  (let ((ABC (make-f64vector 3 0.0)))
				     (pq_coeffs x1 x2 x3 y1 y2 y3 ABC)
				     (values (f64vector-ref ABC 0)
					     (f64vector-ref ABC 1)
					     (f64vector-ref ABC 2)))))
			(+ (* A (* xi xi)) (* B xi) C))))))))


; Include into generated code, but don't parse:
#>

#include <math.h>

double sqr (double x)
{
 return x*x;
}

void pq_coeffs (double x1, double x2, double x3, 
                double y1, double y2, double y3, double *ABC)
{
       
    ABC[0] = ((x2-x1)*y3+(y1-y2)*x3+x1*y2-y1*x2) / ((x2-x1)*sqr(x3)+(sqr(x1)-sqr(x2))*x3+x1*sqr(x2)-sqr(x1)*x2);

    ABC[1] = -((sqr(x2)-sqr(x1))*y3+(y1-y2)*sqr(x3)+sqr(x1)*y2-y1*sqr(x2)) / 
             ((x2-x1)*sqr(x3)+(sqr(x1)-sqr(x2))*x3+x1*sqr(x2)-sqr(x1)*x2);

    ABC[2] = ((x1*sqr(x2)-sqr(x1)*x2)*y3+(y1*x2-x1*y2)*sqr(x3)+(sqr(x1)*y2-y1*sqr(x2))*x3) / 
             ((x2-x1)*sqr(x3)+(sqr(x1)-sqr(x2))*x3+x1*sqr(x2)-sqr(x1)*x2);
}


<#

)
