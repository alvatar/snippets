;;; -*-scheme-*-
;;; ppm.scm
;;;
;;; PPM format according to http://netpbm.sourceforge.net/doc/ppm.html
;;; Simple tool for Gambit-C
;;; 
;;; Copyright 2006-2007 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;; 
;;; $Id: $

;;; (include "matrix.scm")


(define (matrix->ppm-file m filename)

  (let* ((maxval 255)
	 (width  (matrix-columns m))
	 (height (matrix-rows    m))
	 (normalized-m (matrix-normalize-min-max m 1 maxval))
	 (port   (open-output-file filename)))

    ;;;header;;;

    (display "P6"   port) ;; ppm magic number
    (display " "    port)
    (display width  port)
    (display " "    port)
    (display height port)
    (display " "    port)
    (display maxval port)
    (display " "    port)
    
    ;;; write file
    (matrix-for-each (lambda (x)
		       (write-u8 (inexact->exact x) port)
		       (write-u8 (inexact->exact x) port)
		       (write-u8 (inexact->exact x) port))
		     (matrix-map round normalized-m))
    ;; comment out next line for Chicken (but needed for Gambit)
    (close-port port)))
