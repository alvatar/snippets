#!/usr/bin/env gosh

(use gtk)
(use srfi-1)
(use srfi-2)
(use uui.let-objects)

(define (main args)
  (let ((rc  (string-append (sys-getenv "HOME") "/.gtkrc")))
    (if (file-exists? rc) (gtk-rc-add-default-file rc)))
  (gtk-init args)

  (let-objects
   (window :title "Tree View"
           :width-request 400 :height-request 400
           :border-width 2 :destroy (lambda _ (gtk-main-quit))
    (vbox (menu-bar :pack-fill #f :pack-expand #f
           (menu-item :label "File"
            (menu
             (menu-item :label "Quit"
                        :activate (lambda _ (gtk-main-quit))))))
          (hseparator :pack-fill #f :pack-expand #f)
          (scrolled-window
           (hbox
            (tree-view tv
                       :enable-search #t
                       :reorderable #t
                       :data '(("Fruit" "" 0.0
                                ("Apple" "red" 1.0)
                                ("Banana" "yellow" 2.0))
                               ("Vegetable" "" 3.0
                                ("Spinach" "green" 4.0)
                                ("Eggplant" "purple" 5.0)))
             (tree-view-column :name "Name" :sort-column-id 0)
             (tree-view-column :name "Color" :attributes '("text" 1)
                               :sort-column-id 1)
             (tree-view-column :name "Count" :attributes '("text" 2)
                               :sort-column-id 2 :type <real>)
             )))))

   (gtk-tree-view-set-search-column tv 3)
   (tree-view-append-data tv '(("Meat" "" 0.0
                                ("Beef" "red" 1.0)
                                ("Pork" "white" 2.0)
                                ("Human" "the-other-white" 3.0))))

   (gtk-main))

  0)
