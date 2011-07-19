;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: entry.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "GTK Entry" :width-request 200 :height-request 100
           :destroy (lambda _ (gtk-main-quit))
           :delete (lambda _ (gtk-main-quit))
    (vbox
     (entry e :max-length 50 :text "hello"
            :activate (lambda (entry)
                        (format #t "Entry contents: ~a\n"
                                (gtk-entry-get-text entry))))
     (hbox
      (check :label "Editable" :active #t
             :toggled (lambda (check)
                        (gtk-editable-set-editable
                         e (not (zero? (slot-ref check 'active))))))
      (check :label "Visible" :active #t
             :toggled (lambda (check)
                        (gtk-entry-set-visibility
                         e (not (zero? (slot-ref check 'active)))))))
     (button :stock GTK_STOCK_CLOSE :flags GTK_CAN_DEFAULT
             :clicked (lambda _ (gtk-main-quit))
     )))
   (let1 pos (slot-ref e 'text-length)
     (gtk-editable-insert-text e " world" pos))
   (gtk-editable-select-region e 0 (slot-ref e 'text-length))
   (gtk-main))
  0)
