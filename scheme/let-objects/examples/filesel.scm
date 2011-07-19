;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: filesel.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (file-selection filew :title "File selection"
                   :destroy (lambda _ (gtk-main-quit)))
   (g-signal-connect (slot-ref filew 'ok-button)
                     "clicked" (lambda (w)
                                 (format #t "~a\n"
                                         (gtk-file-selection-get-filename filew))))
   (gtk-file-selection-set-filename filew "penguin.png")
   (gtk-main))
  0)
