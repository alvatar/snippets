;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: frame.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window w :title "Frame Example" :border-width 10
           :width-request 300 :height-request 300
           :destroy (lambda _ (gtk-main-quit))
    (frame :label "GTK Frame Widget" :shadow-type GTK_SHADOW_ETCHED_OUT
           :x-align 1.0 :y-align 1.0))
   (gtk-main))
  0)
