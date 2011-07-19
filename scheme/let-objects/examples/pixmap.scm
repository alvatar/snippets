;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: pixmap.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define *xpm-data*
  '("16 16 3 1"
    "       c None"
    ".      c #000000000000"
    "X      c #FFFFFFFFFFFF"
    "                "
    "   ......       "
    "   .XXX.X.      "
    "   .XXX.XX.     "
    "   .XXX.XXX.    "
    "   .XXX.....    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .........    "
    "                "
    "                "))

(define (main args)
  (gtk-init args)
  (let-objects
   (window window :delete (lambda _ (gtk-main-quit)) :border-width 10)
    (let1 style (gtk-widget-get-style window)
      (receive (pixmap mask)
          (gdk-pixmap-create-from-xpm-d (ref window 'window)
                                        (ref (ref style 'bg) GTK_STATE_NORMAL)
                                        *xpm-data*)
        (let-objects
         (button button :clicked (lambda _ (print "button clicked"))
          (pixmap :pixmap pixmap :mask mask))
         (gtk-container-add window button))))
    (gtk-main))
  0)
