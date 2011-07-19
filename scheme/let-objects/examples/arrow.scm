;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: arrow.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use util.let-objects)
(use uui.object-rules)

(define-object-rules (uui-object-rules))

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "Arrow Buttons" :destroy (lambda _ (gtk-main-quit))
           :border-width 10
    (hbox :border-width 2
     (button :pack-expand #f :pack-fill #f :pack-padding 3
      (arrow :arrow-type GTK_ARROW_UP :shadow-type GTK_SHADOW_IN))
     (button :pack-expand #f :pack-fill #f :pack-padding 3
      (arrow :arrow-type GTK_ARROW_DOWN :shadow-type GTK_SHADOW_OUT))
     (button :pack-expand #f :pack-fill #f :pack-padding 3
      (arrow :arrow-type GTK_ARROW_LEFT :shadow-type GTK_SHADOW_ETCHED_IN))
     (button :pack-expand #f :pack-fill #f :pack-padding 3
      (arrow :arrow-type GTK_ARROW_RIGHT :shadow-type GTK_SHADOW_ETCHED_OUT))))
   (gtk-main))
  0)
