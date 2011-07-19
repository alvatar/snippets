;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: base.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window :destroy (lambda _ (gtk-main-quit)))
   (gtk-main))
  0)
