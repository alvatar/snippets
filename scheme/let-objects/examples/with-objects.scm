;;
;; Demonstrating with-objects based on buttons.scm.
;;
;; $Id: with-objects.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

;; alias window
(define-object-rules '((bigboard #f (window))
                       ))

(define (main args)
  (gtk-init args)
  (with-object-rules
   '((masterpiece #f (image))   ;; alias image
     (radio-button #f (hbox)))  ;; redefine radio-button
   (lambda ()
     (let-objects
      (bigboard :title "Pixmap'd Buttons!" :border-width 10
                :destroy (lambda _ (gtk-main-quit))
                :delete (lambda _ (gtk-main-quit))
       (button :clicked (lambda _ (format #t "cool button clicked\n"))
        (radio-button (masterpiece :file "info.xpm")
                      (label :text "cool button"))))
      (gtk-main))))
  0)
