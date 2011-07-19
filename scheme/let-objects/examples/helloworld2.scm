;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: helloworld2.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (callback w button)
  (format #t "Hello again (~s) - ~s was pressed.\n" w button))

(define (delete-event w event)
  (gtk-main-quit))

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "Hello Buttons!" :border-width 10 :delete delete-event
    (hbox
     (button :label "Button 1" :clicked (cut callback <> "button 1"))
     (button :label "Button 2" :clicked (cut callback <> "button 2"))))
  (gtk-main))
  0)
