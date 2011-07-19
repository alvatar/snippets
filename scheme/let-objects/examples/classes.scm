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
   (window :title "Gauche Class Hierarchy"
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
            (tree-store store :value (gtk-tree-store-new <string>))
            (tree-view :model store :data (class-hierarchy)
             (tree-view-column :name "Name" :attributes '("text" 0))
             )))))

   (gtk-main))

  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hierarchy tree code from Kimura Fuyuki

(define (all-classes) (%all-classes (current-module)))

(define (%all-classes module)
  (let ((result '())
        (searched '()))
    (define (search mod)
      (unless (memq mod searched)
        (set! searched (cons mod searched))
        (hash-table-for-each
         (module-table mod)
         (lambda (symbol value)
           (let1 c (eval symbol mod)
             (when (is-a? c <class>)
               (set! result (cons c result))))))))
    (for-each search (module-imports module))
    (for-each search (module-precedence-list module))
    result))

(define (class-hierarchy)
  (make-hierarchy-tree (all-classes) '<top>
                       class-direct-supers
                       class-name))

(define (make-hierarchy-tree lis top parents-of name-of)
  (set! lis (sort lis
                  (lambda (a b)
                    (string<? (symbol->string (name-of a))
                              (symbol->string (name-of b))))))
  (let loop ((node top))
    (cons node
          (filter identity
                  (map (lambda (e)
                         (if (memq node (map name-of (parents-of e)))
                           (loop (name-of e)) #f))
                       lis)))))
