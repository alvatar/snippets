;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: scrolledwin.scm,v 1.1 2002/09/07 05:39:08 shirok Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (dialog window :destroy (lambda _ (gtk-main-quit) #f)
           :title "GtkScrolledWindow example"
           :border-width 0
           :width-request 300 :height-request 300
    ;;(scrolled-window scrolled-window :border-width 10)
    )

   (format #t "window: ~S scroll: ~S\n" window scrolled-window)

   (gtk-scrolled-window-set-policy scrolled-window
                                   GTK_POLICY_AUTOMATIC
                                   GTK_POLICY_ALWAYS)
   (gtk-box-pack-start (ref window 'vbox) scrolled-window #t #t 0)

   (let1 table (gtk-table-new 10 10 #f)
     (gtk-table-set-row-spacings table 10)
     (gtk-table-set-col-spacings table 10)
     (gtk-scrolled-window-add-with-viewport scrolled-window table)
     (gtk-widget-show table)

     (dotimes (i 10)
       (dotimes (j 10)
         (let1 button (gtk-toggle-button-new-with-label
                       (format #f "button (~s,~s)" i j))
           (gtk-table-attach-defaults table button i (+ i 1) j (+ j 1))
           (gtk-widget-show button))))
     )

   (let1 button (gtk-button-new-with-label "close")
     (g-signal-connect button "clicked"
                       (lambda _ (gtk-widget-destroy window)))
     (gtk-widget-set-flags button GTK_CAN_DEFAULT)
     (gtk-box-pack-start (ref window 'action-area) button #t #t 0)
     (gtk-widget-grab-default button)
     (gtk-widget-show button))

   (gtk-main))
  0)
