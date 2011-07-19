;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: buttons.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "Pixmap'd Buttons!" :border-width 10
           :destroy (lambda _ (gtk-main-quit))
           :delete (lambda _ (gtk-main-quit))
    (button :clicked (lambda _ (format #t "cool button clicked\n"))
     (hbox (image :file "info.xpm") (label :text "cool button"))))
   (gtk-main))
  0)
