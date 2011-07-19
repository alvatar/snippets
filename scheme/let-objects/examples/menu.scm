;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: menu.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "GTK Menu Test" :width-request 200 :height-request 100
           :delete (lambda _ (gtk-main-quit))
    (vbox
     (menu-bar :pack-expand #f :pack-fill #f :pack-padding 2
      (menu-item :label "Root Menu"
       (menu menu
        (menu-item :label "Test-undermenu - 1"
                   :activate (lambda _ (print "Test-undermenu - 1")))
        (menu-item :label "Test-undermenu - 2"
                   :activate (lambda _ (print "Test-undermenu - 2")))
        (menu-item :label "Test-undermenu - 3"
                   :activate (lambda _ (print "Test-undermenu - 3"))))))
     (button :label "press me" :pack-padding 2
             :event (lambda (w event)
                      (if (eqv? (ref event 'type) GDK_BUTTON_PRESS)
                        (begin
                          (gtk-menu-popup menu
                                          #f #f #f
                                          (ref event 'button)
                                          (ref event 'time))
                          #t)
                        #f)))))
   (gtk-main))
  0)
