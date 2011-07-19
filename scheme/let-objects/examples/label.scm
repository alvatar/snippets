;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: label.scm,v 1.1 2003/07/05 17:00:39 foof Exp $

(use gtk)
(use uui.let-objects)

(define (main args)
  (gtk-init args)
  (let-objects
   (window :title "Label" :border-width 5 :destroy (lambda _ (gtk-main-quit))
    (hbox :spacing 5
     (vbox :spacing 5
      (frame :label "Normal Label" :pack-expand #f :pack-fill #f
       (label :text "This is a Normal label"))
      (frame :label "Multi-line Label" :pack-expand #f :pack-fill #f
       (label :text "This is a Multi-line label.\nSecond line\nThird line"))
      (frame :label "Left Justified Label" :pack-expand #f :pack-fill #f
       (label :text "This is a Left-Justified\nMulti-line label.\nThird     line"
              :justify GTK_JUSTIFY_LEFT))
      (frame :label "Right Justified Label" :pack-expand #f :pack-fill #f
       (label :text "This is a Right-Justified\nMulti-line label.\nFourth line, (j/k)"
              :justify GTK_JUSTIFY_RIGHT)))
     (vbox :spacing 5
      (frame :label "Line wrapped label" :pack-expand #f :pack-fill #f
       (label :text "This is an example of a line-wrapped label.  It should not be taking up the entire                     width allocated to it, but automatically wraps the words to fit.  The time has come, for all good men, to come to the aid of their party.  The sixth sheik's six sheep's sick.\n     It supports multiple paragraphs correctly, and  correctly  adds many          extra  spaces.  "
              :line-wrap #t))
      (frame :label "Filled, wrapped label" :pack-expand #f :pack-fill #f
       (label :text "This is an example of a line-wrapped, filled label.  It should be taking up the entire                   width allocated to it.  Here is a sentence to prove my point.  Here is another sentence.  Here comes the son, do de do de do.\n    This is a new paragraph.\n    This is another newer, longer, better paragraph.  It is coming to an end, unfortunately."
              :justify GTK_JUSTIFY_FILL :line-wrap #t))
      (frame :label "Underlined label" :pack-expand #f :pack-fill #f
       (label :text "This label is underlined!\nThis one is underlined in quite a funky fasion"
              :pattern "_________________________ _ _________ _ ______     __ _______ ___"
              :justify GTK_JUSTIFY_LEFT)))))
   (gtk-main))
  0)
