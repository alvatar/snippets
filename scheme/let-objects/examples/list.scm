;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: list.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define-constant *list-item-data-key* "list-item-data")

(define (main args)
  (gtk-init args)

  (let-objects
   (window w :title "GtkList Example"
           :destroy (lambda _ (gtk-main-quit))
    (vbox :spacing 5 :border-width 5
      (scrolled-window :width-request 250 :height-request 150
        (list gtklist :selection-changed sigh-print-selection
              :button-release (cut sigh-button-event <> <> f)))
      (frame f :label "Prison" :width-request 200 :height-request 50
             :border-width 5 :shadow-type GTK_SHADOW_OUT)
      (hseparator)
      (button :label "Close" :clicked (lambda _ (gtk-widget-destroy w)))))

   ;; list items
   (dotimes (i 5)
     (let-objects
      (list-item list-item
       (label label :text #`"ListItemContainer with Label #,i"))
      (gtk-container-add gtklist list-item)
      (g-object-set-data list-item *list-item-data-key*
                         (gtk-label-get-text label))))

   ;; more list items, using gtk-list-append-items
   (let ((items '()))
     (dotimes (i 10)
       (let-objects
        (list-item list-item :label #`"List Item with Label ,i")
        (push! items list-item)
        (g-object-set-data list-item *list-item-data-key*
                           "ListItem with integrated Label")))
     (gtk-list-append-items gtklist items))

  (gtk-main))
  0)

(define (sigh-button-event gtklist event frame)
  (when (and (eqv? (slot-ref event 'type) GDK_BUTTON_RELEASE)
             (eqv? (slot-ref event 'button) 3))
    (let* ((selection (slot-ref gtklist 'selection))
           (new-prisoner (if (null? selection) #f (car selection))))
      (for-each (lambda (w)
                  (when (is-a? w <gtk-list-item>)
                    (gtk-widget-reparent w gtklist)))
                (gtk-container-get-children frame))
      (when new-prisoner
        (gtk-list-unselect-child gtklist new-prisoner)
        (gtk-widget-reparent new-prisoner frame))))
  #f)

(define (sigh-print-selection gtklist)
  (let1 selection (slot-ref gtklist 'selection)
    (if (null? selection)
        (print "Selection cleared")
        (format #t "The selection is a ~s\n"
                (map (cut g-object-get-data <> *list-item-data-key*)
                     selection))))
  #f)
