;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: aspectframe.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "Aspect Frame" :destroy (lambda _ (gtk-main-quit))
           :border-width 10
    (aspect-frame :label "2x1" :xalign 0.5 :yalign 0.5 :ratio 2
      ;; Create drawingarea and request it to be 200x200; but the aspect
      ;; frame forces 2x1 aspect, making it 200x100.
      (drawing-area :width-request 200 :height-request 200)))
  (gtk-main))
  0)
