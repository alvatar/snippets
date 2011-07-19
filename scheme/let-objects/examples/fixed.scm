;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: fixed.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let ((x 50) (y 50))
    (let-objects
     (window :title "Fixed Container" :border-width 10
             :destroy (lambda _ (gtk-main-quit))
             (fixed f))
     (dotimes (i 3)
       (let-objects
        (button b :label "Press me"
                :clicked (lambda (w)
                           (set! x (modulo (+ x 30) 300))
                           (set! y (modulo (+ y 50) 300))
                           (gtk-fixed-move f w x y)))
        (gtk-fixed-put f b (* (+ i 1) 50) (* (+ i 1) 50))))
   (gtk-main)))
  0)
