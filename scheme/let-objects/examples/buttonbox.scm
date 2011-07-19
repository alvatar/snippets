;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: buttonbox.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "Button Boxes" :border-width 10
           :destroy (lambda _ (gtk-main-quit))
    (vbox
     (frame :label "Horizontal Button Boxes"
      (vbox :border-width 10
       (frame :label "Spread (spacing 40)"
        (hbutton-box :layout GTK_BUTTONBOX_SPREAD :border-width 5 :spacing 40
         (button :stock GTK_STOCK_OK)
         (button :stock GTK_STOCK_CANCEL)
         (button :stock GTK_STOCK_HELP)))
       (frame :label "Edge (spacing 30)"
        (hbutton-box :layout GTK_BUTTONBOX_EDGE :border-width 5 :spacing 30
         (button :stock GTK_STOCK_OK)
         (button :stock GTK_STOCK_CANCEL)
         (button :stock GTK_STOCK_HELP)))
       (frame :label "Start (spacing 20)"
        (hbutton-box :layout GTK_BUTTONBOX_START :border-width 5 :spacing 20
         (button :stock GTK_STOCK_OK)
         (button :stock GTK_STOCK_CANCEL)
         (button :stock GTK_STOCK_HELP)))
       (frame :label "End (spacing 10)"
        (hbutton-box :layout GTK_BUTTONBOX_END :border-width 5 :spacing 10
         (button :stock GTK_STOCK_OK)
         (button :stock GTK_STOCK_CANCEL)
         (button :stock GTK_STOCK_HELP)))))
     (frame :label "Vertical Button Boxes"
      (hbox :border-width 10
       (frame :label "Spread (spacing 5)"
        (vbutton-box :layout GTK_BUTTONBOX_SPREAD :border-width 5 :spacing 5
         (button :stock GTK_STOCK_OK)
         (button :stock GTK_STOCK_CANCEL)
         (button :stock GTK_STOCK_HELP)))
       (frame :label "Edge (spacing 30)"
        (vbutton-box :layout GTK_BUTTONBOX_EDGE :border-width 5 :spacing 30
         (button :stock GTK_STOCK_OK)
         (button :stock GTK_STOCK_CANCEL)
         (button :stock GTK_STOCK_HELP)))
       (frame :label "Start (spacing 20)"
        (vbutton-box :layout GTK_BUTTONBOX_START :border-width 5 :spacing 20
         (button :stock GTK_STOCK_OK)
         (button :stock GTK_STOCK_CANCEL)
         (button :stock GTK_STOCK_HELP)))
       (frame :label "End (spacing 10)"
        (vbutton-box :layout GTK_BUTTONBOX_END :border-width 5 :spacing 20
         (button :stock GTK_STOCK_OK)
         (button :stock GTK_STOCK_CANCEL)
         (button :stock GTK_STOCK_HELP)))
       ))
     ))
    (gtk-main))
  0)
