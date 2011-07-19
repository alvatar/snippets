;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: table.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (callback data)
  (format #t "Hello again - ~s was pressed\n" data))

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "Table" :delete (lambda _ (gtk-main-quit) #f)
           :border-width 20
    (table :rows 2 :columns 2
     (button :label "button 1" :clicked (lambda (w) (callback "button 1"))
             :attach-left 0 :attach-right 1 :attach-top 0 :attach-bottom 1)
     (button :label "button 2" :clicked (lambda (w) (callback "button 2"))
             :attach-left 1 :attach-right 2 :attach-top 0 :attach-bottom 1)
     (button :label "Quit" :clicked (lambda _ (gtk-main-quit))
             :attach-left 0 :attach-right 2 :attach-top 1 :attach-bottom 2)))
   (gtk-main))
  0)
