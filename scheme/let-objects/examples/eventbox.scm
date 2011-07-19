;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: eventbox.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "Event Box" :border-width 10 :destroy (lambda _ (exit 0))
    (event-box e :events GDK_BUTTON_PRESS_MASK
               :button-press (lambda _ (exit 0))
     (label :text "Click here to quit, quit, quit, quit, quit"
            :width-request 440 :height-request 20)))
   (gdk-window-set-cursor (ref e 'window) (gdk-cursor-new GDK_HAND1))
   (gtk-main))
  0)
