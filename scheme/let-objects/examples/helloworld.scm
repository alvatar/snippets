;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;;  Difference from C version:  Scheme version's signal callback
;;  doesn't take extra "user data": you can use closure if you need
;;  extra data.  With the same reason, there's no 'g-signal-connect-swapped'.
;;
;; $Id: helloworld.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (hello w)
  (format #t "Hello world (~s)\n" w))

(define (destroy w)
  (format #t "Destroying ~s\n" w)
  (gtk-main-quit))

(define (main args)
  (gtk-init args)
  (let-objects
   (window :destroy destroy :border-width 10
    (button :label "Hello world"
            :clicked (lambda (w) (hello w) (destroy w))))
   (gtk-main))
  0)
