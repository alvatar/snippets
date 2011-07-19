;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: radiobuttons2.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "radio buttons" :delete (lambda _ (gtk-main-quit))
    (vbox
     (vbox :spacing 10 :border-width 10
      (radio-group :onchange (lambda (sym) (format #t "~S selected\n" sym))
       (radio :symbol 'vanilla    :label "Vanilla")
       (radio :symbol 'chocolate  :label "Chocolate")
       (radio :symbol 'strawberry :label "Strawberry")))
     (hseparator :pack-fill #f)
     (vbox :spacing 10 :border-width 10
      (button :label "close"
              :clicked (lambda _ (gtk-main-quit))
              :flags GTK_CAN_DEFAULT))))
   (gtk-main))
  0)
