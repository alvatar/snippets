(use fps)

;;; Functional PostScript
;;; Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers

;;; fps-examples.scm

;;; There are ten FPS examples in this file. You can try them
;;; out individually or run "demo" (also an example program) 
;;; which will write output to demo.ps.


;;; ===== Right-angle Fractal ==========================================

;;; This program draws a right-angle fractal. A simple algorithm
;;; generates a very complex picture using just the very basic pt,
;;; line, and compose calls.

(define (fractal point1 point2 depth)
  (stroke (let recur ((pt1 point1) (pt2 point2) (d depth))
	    (if (= d 0) (line pt1 pt2)
		(let* ((x1 (pt:x pt1))
		       (y1 (pt:y pt1))
		       (x2 (pt:x pt2))
		       (y2 (pt:y pt2))
		       (newx (+ (/ (+ x1 x2) 2)
				(/ (- y2 y1) 2)))
		       (newy (- (/ (+ y1 y2) 2)
				(/ (- x2 x1) 2)))
		       (newpt (pt newx newy)))
		  (compose (recur pt1 newpt (- d 1))
			   (recur newpt pt2 (- d 1))))))))


;;; ===== Arrow Fractal =================================================

;;; This program draws arrows recursively. It demonstrate the use of
;;; the composition procedure join and the transformation procedures
;;; translate, scale, and rotate.

;;; The arrow is HEIGHT tall, the arrow-shaft/arrow-head angles are 45
;;; degrees, and the recursive scale factor is 3/5.  This makes it
;;; 1.3258 * HEIGHT wide (in the limit).  It is positioned 10% of
;;; HEIGHT out from the origin -- the lower-left corner of the arrow's
;;; bounding box is at (HEIGHT/10, HEIGHT/10). 

(define (fractal-arrow height depth)
  (let* ((s (/ 3 5))
	 (half-width (/ s	 ; Width of unit arrow / 2.
			(- 1 (* s s))))
	 (arrow1 (unit-arrow-path depth)))
    (stroke (translate (+ (/ (- (inch 8) height) 2) (* (/ 1 2) height))
		       (/ (- (inch 10) height) 2)
		       (scale height height arrow1))
	    (:line-join 'bevel))))


;;; Make a unit fractal-arrow path. The initial shaft of the arrow goes from
;;; (0,0) to (0,1), the shaft/head angles are 45 degrees, and the recursive
;;; scale factor is 3/5. This makes the arrow 1.3258 wide in the limit,
;;; centered on the Y axis. I.e., the arrow's bounding box is
;;;     (-0.663, 0) (0.663, 1)

(define (unit-arrow-path depth)
  (let* ((stem (line (pt 0 0) (pt 0 1)))   ; Basic line
	 (cw  (deg->rad -135))		   ; Clockwise rot angle
	 (ccw (- cw))			   ; Counter clockwise rot angle
	 (s (/ 3 5)))			   ; Scale factor
    (let recur ((depth depth))
      (if (<= depth 1) stem
	  (let ((sub-arrow (scale s s
				  (recur (- depth 1)))))
	    (compose stem 
		     (translate 0 1 (rotate cw  sub-arrow))
		     (translate 0 1 (rotate ccw sub-arrow))))))))

;;; ===== Headlines =================================================

;;; Headlines paints a row of messages in fading color and brightness.
;;; It demonstrate how to create fonts and glyphpaths, how to vary the
;;; style/attribute for drawing in colors, how to use colormap to
;;; create pictures, and how to create and change colors (rgb, hsb,
;;; hsb:h, hsb:b, etc).

;;; The procedure takes one argument, num, which indicates how many
;;; times the headline gets printed.

(define (headlines num)
  (let* ((times (font "Times-BoldItalic" 36))
	 (helv  (font "Helvetica" 36))
	 (headline-path (join (string->glyphpath times  "Turnip")
			      (string->glyphpath helv  " Born Talking")))
	 (headline (paint-glyphpath headline-path (:color (rgb 1 0 0))))
	 (vertical-step 45))

    (translate 100 200
	       (let recur ((n num))
		 (if (<= n 1) headline
		     (let ((next-headline (recur (- n 1))))
		       (compose 			      
			headline
			(translate 0 vertical-step
				   (colormap (lambda (c) 
					       (hsb (hsb:h c)
						    (- (hsb:s c) 0.1)
						    (- (hsb:b c) .05)))
					     (recur (- n 1)))))))))))

				
;;; ===== Clipping Message ==============================================

;;; Clip-msg prints a string of message and fills it with rays. It
;;; demonstrates how to use clip to create pictures. First the ray
;;; picture is drawn, then we create the msg glyphpath, and clip the
;;; ray picture with the path.

;;; The procedure take one argument, msg, which is a simple string
;;; that will be printed.

(define (clip-msg msg)
  (let* ((times  (font "Times-BoldItalic" 84))
	 (m-path (simple-string->glyphpath times msg))
	 (max-pt (bounding-box:max (bounding-box m-path)))
	 (height (pt:y max-pt))
	 (width  (pt:x max-pt))
	 (l-len  (sqrt (+ (* (/ width 2) (/ width 2))
			  (* height height))))
	 (l          (line (pt 0 0) (pt (+ 150 width) 0)))
	 (angle-step .025)
	 (rays (translate (/ width 2) 0
			  (apply compose
				 (let recur ((angle 0))
				   (if (> angle pi) '()
				       (cons (rotate angle l)
					     (recur (+ angle 
						       angle-step)))))))))
    (translate 100 400
	       (clip m-path 
		     (stroke rays (:color (hsb 0.4 0.1 0.4)))))))


;;; ===== Turkey Bitmap ================================================

;;; Turkey bitmap demonstrates how pictures can be created from bitmaps 
;;; (so that it can be passed to the 'show' operator and rendered)
;;; First the bitmap data is read from a hex string, and then the 
;;; bitmap is turned into a picture.

(define turkey-data
  "003B000027000024800E494011492014B2203CB65075FE8817FF8C175F141C07E23803C4703182F8EDFCB2BBC2BB6F8431BFC218EA3C0E3E0007FC0003F8001E18001FF800")

(define (turkey width height)
  (translate (/ (- (inch 8.5) width) 2)
	     (/ (- (inch 11)  height) 2)
	     (scale width height
		    (bitmap->pict (hex-string->bitmap 23 24 1 
						      'gray turkey-data)))))

;;; ===== Sun ==========================================================

;; This procedure draws a sun with n number of spikes around a color
;; wheel (with a disturbingly cute smile). It demonstrates how to 
;; use the with-attrib syntax to temporarily change the default style.

(define (sun n)
  (let* ((radius 50)  ;radius of circle
	 (l 20)       ;length of sun spikes
	 (sat 1)      ;saturation
	 (bri 1)      ;brightness
	 (stick      (line (pt 0 0) (pt radius 0)))
	 (long-stick (line (pt 0 0) (pt (+ radius l) 0)))
	 (full-step  (/ 2pi n))
	 (half-step  (/ full-step 2))
	 (hue-step3  (/ 1 n))
	 (hue-step1  (/ hue-step3 3))
	 (hue-step2  (* hue-step1 2)))
    (with-attrib ((:line-width 2))
      (translate 300 300
		 (compose
		  ;; add disturbing smiley face
		  (with-attrib ((:color (rgb 1 .1 .1)))
		    (compose
				;; right eye
		     (translate (* .3 radius) (* .1 radius)
				(fill (arc origin (* .1 radius) 0 2pi)))
		     ;; left eye
		     (translate (* -.3 radius) (* .1 radius)
				(fill (arc origin (* .1 radius) 0 2pi)))
		     ;; mouth
		     (stroke (arc origin (* .5 radius) pi 2pi))))
		  
		  (let lp ((n n) (hue 0))
		    (let ((spike-pt (end-pt (rotate half-step long-stick))))
		      (if (> n 0)
			  (compose
			   ;; first spike edge
			   (stroke
			    (line (end-pt stick) spike-pt)
			    (:color (hsb (- hue hue-step1) sat bri)))

			   ;; second spike edge
			   (stroke 
			    (line spike-pt (end-pt (rotate full-step stick)))
			    (:color (hsb hue sat bri)))
			   
			   ;; arc section
			   (stroke
			    (arc origin radius 0 full-step)
			    (:color (hsb (- hue hue-step2) sat bri)))
			   
			   ;; the other spikes
			   (rotate full-step (lp (- n 1) (+ hue hue-step3))))
			  
			  ;; recursion ends
			  the-empty-pict))))))))


;;; ===== Bounding Box =================================================

;;; This program draw various arcs and their bounding boxes.
;;; It demonstrates the use of the arc and bounding-box procedures.

(define arc-bounding-box
  (with-attrib ((:line-width 0.01) (:color (rgb 1 1 1)))
     (let* ((square (stroke (rect (pt 0 0) 1 1)))
	    (circle (stroke (arc  (pt (/ 1 2) (/ 1 2)) (/ 1 2) 0 2pi)))
	    (fill-c (rgb 0 0 1))
	    (arc-c  (rgb 1 1 0))
	    (draw-one     
	     (lambda (start end)
	       (let* ((a   (stroke (arc (pt (/ 1 2) (/ 1 2)) (/ 1 2) start end)
				   (:color arc-c) (:line-width 0.04))))
		 (compose (fill (bounding-box->rect (bounding-box a))
				(:color fill-c))
			  circle square a))))
	    (draw-four      
	     (lambda (start)
	       (let lp ((n 0))
		 (if (= n 4)
		     the-empty-pict
		     (compose (draw-one start (+ start (* n 1/2pi) 1/4pi))
			      (translate 0 1.5 (lp (+ n 1)))))))))
       (lambda ()
	 (compose 
	  (fill (rect (pt 0 0) (inch 8.5) (inch 11)))
	  (translate 109 144
		     (scale 72 72
			    (let lp ((start 0))
			      (if (>= start 2pi)
				  the-empty-pict
				  (compose (draw-four start)
					   (translate 1.5 0
						      (lp (+ start 
							     1/2pi)))))))))))))

;;; ===== Morphing Square to Circle =====================================

;;; This program "morphes" a square to a circle in n steps. It demonstrates
;;; the use of tangent-arc procedure and the dash attribute.

(define square-to-circle
  (let ((draw-one (lambda (r c)
		    (let ((a (tangent-arc (pt 36 0) (pt 36 36) (pt 0 36) r)))
		      (stroke (close-path (link a
						(rotate 1/2pi a)
						(rotate pi a) 
						(rotate 3/2pi a)))
			      (:color c)
			      (:line-width 3)
			      (:dash-pattern '#(4 3)))))))
    (lambda (n)
      (translate (inch 1) (inch 5)
		 (let ((c-step (/ 1 n))
		       (r-step (/ 36 n))
		       (h-step (/ (inch 8) n)))
		   (let lp ((n n) (r 36) (c (rgb 1 0 0)))
		     (if (= n 0)
			 the-empty-pict
			 (compose (draw-one r c)
				  (translate h-step 0
					     (lp (- n 1) 
						 (- r r-step)
						 (rgb (- (rgb:r c) c-step)
						      (+ (rgb:g c) c-step)
						      0)))))))))))

;;; ===== Text Along a Circle ==========================================

;;; This program puts two strings of text around a circle.
;;; It takes four argument, top-string, top-space, bot-string, bot-space.
;;; top-string and bot-string are the text strings. top-space and bot-space
;;; are numbers that are multiples of the width of the blank space glyph.

(define circle-text 
  (let* ((radius      200)
	 (font        (font "Times-Roman" 48))
	 (space-w     (pt:x (end-pt (char->glyphpath font #\space))))
	 (into-position-top (lambda (g) (translate radius 0 (rotate (- 1/2pi) g))))
	 (into-position-bot (lambda (g) 
			      (translate (+ radius 34) 0 (rotate 1/2pi g)))))
    
    (lambda (text-top n-top text-bot n-bot)
      (let* ((top-lst   (map (lambda (c) (char->glyphpath font c))
			     (string->list text-top)))
	     (bot-lst   (map (lambda (c) (char->glyphpath font c))
			     (string->list text-bot)))
	     (single-angle  (lambda (g n) (let ((w (pt:x (end-pt g))))
					    (* 2 (tan (/ (/ (+ w (* space-w n)) 2)
							 radius))))))
	     (top-angle (fold (lambda (g angle) (+ (single-angle g n-top) angle))
			      0 top-lst))
	     (bot-angle (fold (lambda (g angle) (+ (single-angle g n-bot) angle))
			      0 bot-lst)))
	(translate 
	 300 400
	 (stroke
	  (compose
	   (arc (pt 0 0) (- radius 24) 0 2pi)
	   (arc (pt 0 0) (+ radius 56) 0 2pi)
	   (apply compose
		  (let lp ((top-lst top-lst) 
			   (angle (+ top-angle (/ (- pi top-angle) 2))))
		    (if (null? top-lst)
			'()
			(cons (rotate angle (into-position-top (car top-lst)))
			      (lp (cdr top-lst) 
				  (- angle (single-angle 
					    (car top-lst) n-top)))))))
	   (apply compose
		  (let lp ((bot-lst bot-lst) 
			   (angle (- (- 2pi bot-angle) 
				     (/ (- pi bot-angle) 2))))
		    (if (null? bot-lst)
			'()
			(cons (rotate angle (into-position-bot (car bot-lst)))
			      (lp (cdr bot-lst) 
				  (+ angle (single-angle 
					    (car bot-lst) n-bot))))))))))))))


;;; ===== Demo Driver ==================================================

;;; This is the demo driver for the FPS sample file. It demonstrates
;;; the use of show, channel, and various options. It outputs to
;;; the file demo.ps.

(define (demo)
  (let* ((channel (ps2-text-channel "demo.ps" (:title "FPS Demo")))
	 (doit    (lambda (pic label) 
		    (show channel pic (:page-label label))
		    (display label) (newline))))
    (doit (fractal (pt 150 350) (pt 275 575) 10) "basic-lines")
    (doit (fractal-arrow 400 9)                  "transformation")
    (doit (headlines 9)                          "colormap")
    (doit (clip-msg "Scheme rules.")             "clipping")
    (doit (turkey 144 144)                       "bitmap")
    (doit (arc-bounding-box)                     "bounding-box")
    (doit (sun 12)                               "sun")
    (doit (square-to-circle 6)                   "tangent-arc")
    (doit (circle-text "Functional PostScript" 0.3 
		       "Halloween 1996" 0.5)       "circle-text")
    (close-channel channel)))


;;; ===== Simple Picture Test Driver ====================================

;;; A simple test driver that outputs to test.ps.

(define (test pict)
  (show-w/ps2-text-channel "test.ps" pict))

;;; ===== End of fps-examples.scm =======================================



(demo)
