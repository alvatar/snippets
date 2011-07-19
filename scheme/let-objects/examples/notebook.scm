;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: notebook.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (rotate-book notebook)
  (gtk-notebook-set-tab-pos notebook
                            (modulo (+ (ref notebook 'tab-pos) 1) 4)))

(define (tabsborder-book notebook)
  (let ((tval (zero? (ref notebook 'show-tabs)))
        (bval (zero? (ref notebook 'show-border))))
    (gtk-notebook-set-show-tabs notebook tval)
    (gtk-notebook-set-show-border notebook bval)))

(define (remove-book notebook)
  (let1 page (gtk-notebook-get-current-page notebook)
    (gtk-notebook-remove-page notebook page)
    (gtk-widget-queue-draw notebook)))

(define (main args)
  (gtk-init args)
  (let-objects
   (window :delete (lambda _ (gtk-main-quit)) :border-width 10
    (table table :rows 3 :columns 6
     (notebook notebook :tab-pos GTK_POS_TOP :attach-right 6 :attach-bottom 1)
     (button :label "close"
             :attach-left 0 :attach-right 1 :attach-top 1 :attach-bottom 2
             :clicked (lambda _ (gtk-main-quit)))
     (button :label "next page"
             :attach-left 1 :attach-right 2 :attach-top 1 :attach-bottom 2
             :clicked (lambda _ (gtk-notebook-next-page notebook) #t))
     (button :label "prev page"
             :attach-left 2 :attach-right 3 :attach-top 1 :attach-bottom 2
             :clicked (lambda _ (gtk-notebook-prev-page notebook) #t))
     (button :label "tab position"
             :attach-left 3 :attach-right 4 :attach-top 1 :attach-bottom 2
             :clicked (lambda _ (rotate-book notebook) #t))
     (button :label "tabs/border on/off"
             :attach-left 4 :attach-right 5 :attach-top 1 :attach-bottom 2
             :clicked (lambda _ (tabsborder-book notebook) #t))
     (button :label "remove page"
             :attach-left 5 :attach-right 6 :attach-top 1 :attach-bottom 2
             :clicked (lambda _ (remove-book notebook) #t))))

   (dotimes (i 5)
     (let-objects
      (frame frame :label #`"Append Frame ,(+ i 1)" :border-width 10
             :width-request 100 :height-request 75
       (label :text #`"Append Frame ,(+ i 1)"))
      (let1 label (gtk-label-new #`"Page ,(+ i 1)")
        (gtk-notebook-append-page notebook frame label))))

   (let-objects
    (check cb :label "Check me please!" :width-request 100 :height-request 75)
    (let1 label (gtk-label-new "Add page")
      (gtk-notebook-insert-page notebook cb label 2)))

   (dotimes (i 5)
     (let-objects
       (frame frame :label #`"Prepend Frame ,(+ i 1)"
              :border-width 10 :width-request 100 :height-request 75
         (label :text #`"Prepend Frame ,(+ i 1)"))
      (let1 label (gtk-label-new #`"PPage ,(+ i 1)")
        (gtk-notebook-prepend-page notebook frame label))))

   (gtk-notebook-set-current-page notebook 3)
   (gtk-main))
  0)
