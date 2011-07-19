;;;; uui.object-rules -- GTK widget utilities
;;
;; Copyright (c) 2003 by Alex Shinn <foof@synthcode.com>
;;
;; This library is free software; you can redistribute it and/or modify
;; it under either the GNU GPL or Artistic licenses.

(define-module uui.object-rules
  (use gtk)
  (use srfi-1)
  (use srfi-2)
  (use srfi-11)
  (use gauche.parameter)
  (use util.streams)
  (use util.let-objects)
  (export <uui-radio-group> radio-group-get-selection
          tree-view-clear-data tree-view-set-data tree-view-append-data
          tree-view-update-data tree-view-append-data-stream
          tree-view-find tree-view-member tree-view-update! tree-view-for-each
          make-file-chooser set-tooltips enable-tooltips disable-tooltips
          debug-event
          uui-object-rules))
(select-module uui.object-rules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes

(define-class <uui-radio-group> ()
  ((children :initform '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

;; when you just want an "i was called" callback
(define (debug-event . args)
  (format (current-error-port) "event caught: ~S\n" args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tooltips

(define *uui-tooltips* (delay (gtk-tooltips-new)))

(define (set-tooltips wgt tip)
  (let ((text (if (pair? tip) (car tip) tip))
        (private (or (and (pair? tip) (pair? (cdr tip)) (cadr tip)) "")))
    (gtk-tooltips-set-tip (force *uui-tooltips*) wgt text private)))

(define (enable-tooltips)  (gtk-tooltips-enable (force *uui-tooltips*)))
(define (disable-tooltips) (gtk-tooltips-disable (force *uui-tooltips*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specific object helpers

(define (radio-group-get-selection gr)
  (let loop ((kids (slot-ref gr 'children)))
    (if (null? kids)
      #f
      (let ((kid1 (car kids)))
        (if (gtk-toggle-button-get-active (car kid1))
          (cadr kid1)
          (loop (cdr kids)))))))

(define (make-file-chooser handler . keys)
  (let-keywords* keys ((title "Choose File")
                       (default "")
                       (complete #f)
                       (multiple #f))
    (let ((filew (gtk-file-selection-new title)))
      (if multiple
        (begin
          (gtk-file-selection-set-select-multiple filew #t)
          (g-signal-connect
              (slot-ref filew 'ok-button)
              "clicked" (lambda (w)
                          (handler (gtk-file-selection-get-selections filew))
                          (gtk-widget-hide filew))))
        (g-signal-connect
            (slot-ref filew 'ok-button)
            "clicked" (lambda (w)
                        (handler (gtk-file-selection-get-filename filew))
                        (gtk-widget-hide filew))))
      (g-signal-connect (slot-ref filew 'cancel-button)
          "clicked" (lambda (w) (gtk-widget-hide filew)))
      (lambda _
        (gtk-file-selection-set-filename filew default)
        (gtk-widget-show filew)
        (when complete
          (gtk-file-selection-complete filew complete) )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general widget hierarchy tools

(define (find-ancestor-of-class obj class)
  (let ((parent (slot-ref obj 'parent)))
    (cond ((not parent)
           #f)
          ((is-a? parent class)
           parent)
          (else
           (find-ancestor-of-class parent class)))))

(define (map-children proc obj)
  (let ((res '()))
    (let loop ((kids (list obj)))
      (if (null? kids)
        #f
        (let ((kid1 (car kids)))
          (push! res (proc kid1))
          (when (is-a? kid1 <gtk-container>)
            (loop (gtk-container-get-children kid1)))
          (loop (cdr kids)))))
    (reverse! res)))

;; coerce text entry to other types
(define (type-coercer type)
  (cond
    ((eq? type <string>)  identity)
    ((eq? type <integer>) x->integer)
    ((eq? type <number>)  x->number)
    ((eq? type <real>)    (compose exact->inexact x->number))
    (else (error "unknown entry type: " type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-view hierarchy tools

;; find an iter in a tree view

(define (tree-view-find pred tree)
  (and-let* ((store (gtk-tree-view-get-model tree))
             (first (gtk-tree-model-get-iter-first store)))
    (let loop ((iter first))
      (if (pred iter)
        iter
        (let ((kid (gtk-tree-model-iter-children store iter))
              (sib? (gtk-tree-model-iter-next store iter)))
          (or (and kid (loop kid))
              (and sib? (loop iter))))))))

;; find first iter matching a value, optionally from a specific column

(define (tree-view-member tree col key . opt-eq)
  (let ((store (gtk-tree-view-get-model tree))
        (eq (get-optional opt-eq equal?)))
    (if (number? col)
      (tree-view-find
       (lambda (i) (eq key (gtk-tree-model-get-value store i col)))
       tree)
      (let ((cols (iota (tree-view-get-num-columns tree))))
        (tree-view-find
         (lambda (i)
           (any (lambda (col) (eq key (gtk-tree-model-get-value store i col)))
                cols))
         tree)))))

;; update a row in the tree matching on a key

(define (tree-view-update! tree col vals . opt-eq)
  (and-let* ((eq (get-optional opt-eq equal?))
             (store (gtk-tree-view-get-model tree))
             (iter (tree-view-member tree col (list-ref vals col) eq))
             (n (tree-view-get-num-columns tree)))
    (let loop ((ls vals)
               (i 0))
      (when (pair? ls)
        (unless (= i col)
          (gtk-tree-store-set-value store iter i (car ls)))
        (loop (cdr ls) (+ i 1))))))

;; iterating

(define (tree-view-for-each proc tree)
  (and-let* ((store (gtk-tree-view-get-model tree))
             (first (gtk-tree-model-get-iter-first store)))
    (let loop ((iter first))
      (proc iter)
      (let ((kid (gtk-tree-model-iter-children store iter))
            (sib? (gtk-tree-model-iter-next store iter)))
        (if kid (loop kid))
        (if sib? (loop iter))))))

(define (tree-view-get-num-columns tree)
  (length (gtk-tree-view-get-columns tree)))

;; setting data from lists

(define (tree-view-clear-data tree)
  (and-let* ((store (gtk-tree-view-get-model tree)))
    (gtk-tree-store-clear store)))

(define (tree-view-append-data tree data)
  (let ((store (gtk-tree-view-get-model tree))
        (ncols (tree-view-get-num-columns tree)))
    (let loop ((tr data)
               (parent #f))
      (when (pair? tr)
        (let* ((element (car tr))
               (iter (gtk-tree-store-append store parent)))
          (let loop2 ((el element) (i 0))
            (when (pair? el)
              (cond ((>= i ncols)
                     (loop el iter))
                    (else
                     (gtk-tree-store-set-value store iter i (car el))
                     (loop2 (cdr el) (+ i 1))))))
          (loop (cdr tr) parent))))))

(define (tree-view-set-data tree data)
  (tree-view-clear-data tree)
  (tree-view-append-data tree data))

(define (tree-view-append-data-stream tree data)
  (let* ((store (gtk-tree-view-get-model tree))
         (ncols (tree-view-get-num-columns tree))
         (tree-h (tree-view-unseen-height tree))
         (row-h 8)
         (delta 100)
         (rows-to-add (/ (+ tree-h delta) row-h)))
    ;;(format #t "ncols: ~S h: ~S rows-to-add: ~S\n" ncols tree-h rows-to-add)
    (let loop ((tr data)
               (parent #f)
               (add-row-count 0))
      (cond ((and (not (stream-null? tr))
                  (< add-row-count rows-to-add))
             (let* ((element (stream-car tr))
                    (iter (gtk-tree-store-append store parent)))
               (let loop2 ((el element) (i 0))
                 ;;(format #t "(loop2 ~S)\n" el)
                 (when (pair? el)
                   (cond ((>= i ncols)
                          (loop el iter))
                         (else
                          (gtk-tree-store-set-value store iter i (car el))
                          (loop2 (cdr el) (+ i 1))))))
               ;; how do you force a refresh so tree-view-unseen-height
               ;; accounts for the newly added rows?
               ;;(gtk-widget-realize (find-ancestor-of-class tree <gtk-scrolled-window>))
               ;;(gtk-widget-realize tree)
               ;;(gtk-widget-show-now tree)
               (loop (stream-cdr tr) parent (+ add-row-count 1))))
            (else
             ;; return data when done
             tr)))))

(define (tree-view-unseen-height tree)
  (and-let* ((sc (find-ancestor-of-class tree <gtk-scrolled-window>))
             (va (gtk-scrolled-window-get-vadjustment sc))
             (r (make <gdk-rectangle>)))
    (gtk-tree-view-get-visible-rect tree r)
    (let-values (((x y) (gtk-widget-translate-coordinates
                         tree sc
                         (slot-ref r 'width)
                         (slot-ref r 'height))))
      ;;(format #t "upper: ~S value: ~S y: ~S r-height: ~S\n"
      ;;        (slot-ref va 'upper) (slot-ref va 'value) y (slot-ref r 'height))
      ;; XXXX what's the right computation here?
      (- (slot-ref va 'upper) (+ y (slot-ref va 'value)) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object hierarchy and backend definition

(define uui-object-rules
  (make-parameter

   `(
     (gtk-object ,<gtk-object> (object)
      (properties ("user-data" 'read 'write))
      (signals ("destroy" :destroy)))

     (widget ,<gtk-widget> (gtk-object)
      (init
       ,(lambda (obj parent keys)
          ;; connect to parent if exists
          (if parent
            (and-let* ((appender (object-class-get-slot parent 'append))
                       (vals (map (lambda (arg)
                                    (let* ((sym (car arg))
                                           (key (make-keyword sym)))
                                      (get-key
                                       key keys
                                       (get-key :default (cdr arg)))))
                                  (if (pair? (cdr appender))
                                    (cadr appender) '()))))
              (apply (car appender) (append (list parent obj) vals))))
          ;; handle signals
          (for-each
           (lambda (s)
             (and-let* ((val (get-key-ls (cadr s) keys)))
               ;;(format #t "connecting signal ~S\n" (car s))
               (g-signal-connect obj (car s) (car val))))
           (object-class-get-multi obj 'signals)))
       )
      (post-init
       ,(lambda (obj parent keys key-hash kids)
          ;; show unless specified otherwise
          (unless (get-key-ls :noshow keys)
            (if (is-a? obj <gtk-widget>) (gtk-widget-show obj)))
          ))
      (keywords (,gtk-widget-set-flags #f :flags)
                (,gtk-widget-set-events #f :events)
                (,gtk-widget-set-size-request #f :width-request
                                              :height-request)
                (,set-tooltips #f :tooltips)
                )
      (properties ("app-paintable" read write)
                  ("can-default" read write)
                  ("can-focus" read write)
                  ("composite-child" read)
                  ("events" read write)
                  ("extension-events" read write)
                  ("has-default" read write)
                  ("has-focus" read write)
                  ("height-request" read write)
                  ("is-focus" read write)
                  ("name" read write)
                  ("parent" read write)
                  ("receives-default" read write)
                  ("sensitive" read write)
                  ("style" read write)
                  ("visible" read write)
                  ("width-request" read write)
                  )
      (signals ("accel-closures-changed" :accel-closures-changed)
               ("button_press_event" :button-press)
               ("button_release_event" :button-release)
               ("child-notify" :child-notify)
               ("client-event" :client-event)
               ("configure-event" :configure-event)
               ("delete_event" :delete)
               ("destroy" :destroy)
               ("direction-changed" :direction-changed)
               ("drag-begin" :drag-begin)
               ("drag-data-delete" :drag-data-delete)
               ("drag-data-get" :drag-data-get)
               ("drag-data-received" :drag-data-received)
               ("drag-drop" :drag-drop)
               ("drag-end" :drag-end)
               ("drag-leave" :drag-leave)
               ("drag-motion" :drag-motion)
               ("enter-notify-event" :enter-notify-event)
               ("event" :event)
               ("event-after" :event-after)
               ("expose-event" :expose-event)
               ("focus" :focus)
               ("focus-in-event" :focus-in-event)
               ("focus-out-event" :focus-out-event)
               ("grab-focus" :grab-focus)
               ("grab-notify" :grab-notify)
               ("hide" :hide)
               ("hierarchy-changed" :hierarchy-changed)
               ("key-press-event" :key-press-event)
               ("key-release-event" :key-release-event)
               ("leave-notify-event" :leave-notify-event)
               ("map" :map)
               ("map-event" :map-event)
               ("mnemonic-activate" :mnemonic-activate)
               ("motion-notify-event" :motion-notify-event)
               ("no-expose-event" :no-expose-event)
               ("parent-set" :parent-set)
               ("popup-menu" :popup-menu)
               ("property-notify-event" :property-notify-event)
               ("proximity-in-event" :proximity-in-event)
               ("proximity-out-event" :proximity-out-event)
               ("realize" :realize)
               ("scroll-event" :scroll-event)
               ("selection-clear-event" :selection-clear-event)
               ("selection-get" :selection-get)
               ("selection-notify-event" :selection-notify-event)
               ("selection-received" :selection-received)
               ("selection-request-event" :selection-request-event)
               ("show" :show)
               ("show-help" :show-help)
               ("size-allocate" :size-allocate)
               ("size-request" :size-request)
               ("state-changed" :state-changed)
               ("style-set" :style-set)
               ("unmap" :unmap)
               ("unmap-event" :unmap-event)
               ("unrealize" :unrealize)
               ("visibility-notify-event" :visibility-notify-event)
               ("window-state-event" :window-state-event)
               )
      )

     ;; containers
     (container ,<gtk-container> (widget)
      (append ,gtk-container-add)
      (keywords (,gtk-container-set-border-width #f :border-width)
                (,gtk-container-set-resize-mode #f :resize-mode))
      (properties ("border-width" read write)
                  ("child" write)
                  ("resize-mode" read write))
      (signals ("add" :add)
               ("check-resize" :check-resize)
               ("remove" :remove)
               ("set-focus-child" :set-focus-child)))
     (bin ,<gtk-bin> (container))

     (alignment ,<gtk-alignment> (bin)
      (make ,gtk-alignment-new))
     (frame ,<gtk-frame> (bin)
      (make ,gtk-frame-new ((label :default "")))
      (keywords (,gtk-frame-set-shadow-type #f :shadow-type)
                (,gtk-frame-set-label-align #f :x-align :y-align)))
     (aspect-frame ,<gtk-aspect-frame> (frame)
      (make ,gtk-aspect-frame-new ((label :default "")
                                   (xalign :default 1.0)
                                   (yalign :default 1.0)
                                   (ratio :default 1)
                                   (obey-child :default #f))))
     (event-box ,<gtk-event-box> (bin)
      (make ,gtk-event-box-new)
      (post-init
       ,(lambda (obj parent keys key-hash start-kids)
          (gtk-widget-realize obj))))
     (handle-box ,<gtk-handle-box> (bin)
      (make ,gtk-handle-box-new))
     (scrolled-window ,<gtk-scrolled-window> (bin)
      (make ,gtk-scrolled-window-new ((hadjustment :default #f)
                                      (vadjustment :default #f)))
      (append ,gtk-scrolled-window-add-with-viewport)
      (keywords (,gtk-scrolled-window-set-policy #f :horizontal-policy
                                                 :vertical-policy))
      (signals ("move-focus-out" :move-focus-out)
               ("scroll-child" :scroll-child)))
     (viewport ,<gtk-viewport> (bin)
      (make ,gtk-viewport-new))

     ;; windows
     (window ,<gtk-window> (bin)
      (make ,gtk-window-new ((parent :default ,GTK_WINDOW_TOPLEVEL)))
      (properties ("title" read write))
      (keywords (,gtk-window-set-title #f :title)
                (,gtk-window-resize #f :width :height)))

     (dialog ,<gtk-dialog> (window)
      (make ,gtk-dialog-new))
     (color-selection-dialog ,<gtk-color-selection-dialog> (dialog)
      (make ,gtk-color-selection-dialog-new))
     (file-selection ,<gtk-file-selection> (dialog)
      (make ,gtk-file-selection-new ((title :default ""))))
     (font-selection-dialog ,<gtk-font-selection-dialog> (dialog)
      (make ,gtk-font-selection-dialog-new))
     (input-dialog ,<gtk-input-dialog> (dialog)
      (make ,gtk-input-dialog-new))
     (message-dialog ,<gtk-message-dialog> (dialog))
     (plug ,<gtk-plug> (window))

     ;; buttons
     (button ,<gtk-button> (bin)
      (make ,gtk-button-new)
      (init
       ;; handle implicit labels or stock buttons
       ,(lambda (obj parent keys)
          (or (and-let* ((label-text (get-key :label keys))
                         (label (gtk-label-new label-text)))
                (gtk-widget-show label)
                (gtk-container-add obj label))
              (and-let* ((stock-value (get-key :stock keys))
                         (stock (gtk-button-new-from-stock stock-value)))
                (gtk-widget-reparent (car (gtk-container-get-children stock)) obj))))
       )
      (properties ("label" read write construct)
                  ("relief" read write)
                  ("use-stock" read write construct)
                  ("use-underline" read write construct))
      (signals ("activate" :activate)
               ("clicked" :clicked)
               ("enter" :enter)
               ("leave" :leave)
               ("pressed" :pressed)
               ("release" :released)))
     (toggle-button ,<gtk-toggle-button> (button)
      (make ,gtk-toggle-button-new)
      (keywords (,gtk-toggle-button-set-active #f :active))
      (signals ("toggled" :toggled)))
     (check-button ,<gtk-check-button> (toggle-button)
      (make ,gtk-check-button-new))
     (radio-button ,<gtk-radio-button> (check-button)
      (make ,gtk-radio-button-new
        ((constructor-group :default #f)))
      (init
       ;; handle explicit group setting
       ,(lambda (obj parent keys)
          (and-let* ((group-ls (get-key-ls :group keys))
                     (group (force (car group-ls)))
                     (gr (if (is-a? group <gtk-radio-group>)
                           group
                           (gtk-radio-button-get-group group))))
            (gtk-radio-button-set-group obj gr)))
       ))
     ;; short names
     (toggle #f (toggle-button))
     (check #f (check-button))
     (radio #f (radio-button))

     ;; not quite a <gtk-radio-group>... we want something that can tell
     ;; us the current selection
     (radio-group ,<uui-radio-group> (object)
      (make ,(cut make <uui-radio-group>))
      (post-init
       ,(lambda (obj parent keys key-hash start-kids)
          ;;(format #t "post-init radio-group: ~S ~S\n" obj start-kids)
          (let ((gr #f)
                (tog (or (get-key :toggled keys)
                         (and-let* ((oc (get-key :onchange keys)))
                           (lambda (e)
                             (when (gtk-toggle-button-get-active e)
                               (oc (radio-group-get-selection obj))))))))
            (for-each
             (cut map-children
              (lambda (kid)
                (when (is-a? kid <gtk-radio-button>)
                  ;;(format #t "found a radio button\n")
                  (if gr
                    (gtk-radio-button-set-group kid gr)
                    (set! gr (gtk-radio-button-get-group kid)))
                  (if tog (g-signal-connect kid "toggled" tog))
                  (let ((sym (get-key :symbol (hash-table-get key-hash kid '()) #f))
                        (radio-kids (slot-ref obj 'children)))
                    (slot-set! obj 'children (cons (list kid sym) radio-kids)))))
              <>)
             start-kids)))))

     ;; boxes
     (box ,<gtk-box> (container)
      (append ,gtk-box-pack-start
              ((pack-expand :default #t)
               (pack-fill :default #t)
               (pack-padding :default 0)))
      (keywords (,gtk-box-set-spacing #f :spacing)
                (,gtk-box-set-homogeneous #f :homogeneous)))
     (button-box ,<gtk-button-box> (box)
      (keywords (,gtk-button-box-set-layout #f :layout)))
     (hbutton-box ,<gtk-hbutton-box> (button-box)
      (make ,gtk-hbutton-box-new))
     (vbutton-box ,<gtk-vbutton-box> (button-box)
      (make ,gtk-vbutton-box-new))
     (vbox ,<gtk-vbox> (box)
      (make ,gtk-vbox-new ((homogeneous :default #f) (spacing :default 0))))
     (color-selection ,<gtk-color-selection> (vbox)
      (make ,gtk-color-selection-new))
     (font-selection ,<gtk-font-selection> (vbox)
      (make ,gtk-font-selection-new))
     (hbox ,<gtk-hbox> (box)
      (make ,gtk-hbox-new ((homogeneous :default #f) (spacing :default 0))))
     (combo ,<gtk-combo> (hbox)
      (make ,gtk-combo-new))
     (statusbar ,<gtk-statusbar> (hbox)
      (make ,gtk-statusbar-new))

     ;; menus
     (menu-shell ,<gtk-menu-shell> (container))
     (menu-bar ,<gtk-menu-bar> (menu-shell)
      (make ,gtk-menu-bar-new)
      (append ,gtk-menu-shell-append)
      )
     (menu ,<gtk-menu> (menu-shell)
      (make ,gtk-menu-new)
      (append ,gtk-menu-shell-append)
      )

     ;; other containers
     (fixed ,<gtk-fixed> (container)
      (make ,gtk-fixed-new)
      (append ,gtk-fixed-put ((fix-x :default 0) (fix-y :default 0))))
     (paned ,<gtk-paned> (container))
     (hpaned ,<gtk-hpaned> (paned)
      (make ,gtk-hpaned-new))
     (vpaned ,<gtk-vpaned> (paned)
      (make ,gtk-vpaned-new))
     (layout ,<gtk-layout> (container)
      (make ,gtk-layout-new))
     (notebook ,<gtk-notebook> (container)
      (make ,gtk-notebook-new)
      (keywords (:tab-pos #f ,gtk-notebook-set-tab-pos)))
     (socket ,<gtk-socket> (container)
      (make ,gtk-socket-new))
     (table ,<gtk-table> (container)
      (make ,gtk-table-new ((rows :default 1)
                            (columns :default 1)
                            (homogeneous :default #t)))
      (append ,gtk-table-attach-defaults
              ((attach-left :default 0)
               (attach-right :default 0)
               (attach-top :default 0)
               (attach-bottom :default 0))))
     (text-view ,<gtk-text-view> (widget)
      (make ,gtk-text-view-new)
      (keywords (,gtk-text-view-set-editable
                 ,gtk-text-view-get-editable :editable)
                (,gtk-text-view-set-cursor-visible
                 ,gtk-text-view-get-cursor-visible :cursor-visible)
                (,gtk-text-view-set-wrap-mode
                 ,gtk-text-view-get-wrap-mode :wrap-mode)
                (,gtk-text-view-set-justification
                 ,gtk-text-view-get-justification :justification)
                (,gtk-text-view-set-pixels-above-lines
                 ,gtk-text-view-get-pixels-above-lines :pixels-above-lines)
                (,gtk-text-view-set-pixels-below-lines
                 ,gtk-text-view-get-pixels-below-lines :pixels-below-lines)
                (,gtk-text-view-set-left-margin
                 ,gtk-text-view-get-left-margin :left-margin)
                (,gtk-text-view-set-right-margin
                 ,gtk-text-view-get-right-margin :right-margin)
                (,gtk-text-view-set-pixels-inside-wrap
                 ,gtk-text-view-get-pixels-inside-wrap :inside-wrap)
                (,gtk-text-view-set-indent
                 ,gtk-text-view-get-indent :indent)
                ))
     (toolbar ,<gtk-toolbar> (container)
      (make ,gtk-toolbar-new))

     (tree-view ,<gtk-tree-view> (container)
      (make ,gtk-tree-view-new)
      (append ,gtk-tree-view-append-column)
      (post-init
       ,(lambda (obj parent keys key-hash start-kids)
          (unless (gtk-tree-view-get-model obj)
            (let* ((n (tree-view-get-num-columns obj))
                   (types (map (lambda (w)
                                 (get-key :type (hash-table-get key-hash w '())
                                          <string>))
                               (filter (cut is-a? <> <gtk-tree-view-column>)
                                       (append-map
                                        (cut map-children identity <>)
                                        start-kids))))
                   (store (apply gtk-tree-store-new types)))
              (gtk-tree-view-set-model obj store)))
          (and-let* ((data (get-key :data keys)))
            (tree-view-set-data obj data))
          (and-let* ((data (get-key :data-stream keys)))
            ;; first fill some data
            (dotimes (i 100)
              (tree-view-append-data obj (list (stream-car data)))
              (set! data (stream-cdr data)))
            ;; then setup the data to refill on scrolling
            (and-let* ((sc (find-ancestor-of-class obj <gtk-scrolled-window>))
                       (va (gtk-scrolled-window-get-vadjustment sc)))
              (g-signal-connect
               va "value-changed"
               (lambda (a)
                 (set! data (tree-view-append-data-stream obj data))))))))
      (keywords (,gtk-tree-view-set-model ,gtk-tree-view-set-model :model)
                (,gtk-tree-view-set-hadjustment
                 ,gtk-tree-view-get-hadjustment :hadjustment)
                (,gtk-tree-view-set-vadjustment
                 ,gtk-tree-view-get-vadjustment :vadjustment)
                (,gtk-tree-view-set-headers-visible
                 ,gtk-tree-view-get-headers-visible :headers-visible)
                (,gtk-tree-view-set-headers-clickable #f :headers-clickable)
                (,gtk-tree-view-set-rules-hint
                 ,gtk-tree-view-get-rules-hint :rules-hint)
                (,gtk-tree-view-set-expander-column #f :expander-column)
                (,gtk-tree-view-set-reorderable
                 ,gtk-tree-view-get-reorderable :reorderable)
                (,gtk-tree-view-set-enable-search
                 ,gtk-tree-view-get-enable-search :enable-search)
                (,gtk-tree-view-set-search-column
                 ,gtk-tree-view-get-search-column :search-column)
                )
      (properties ("enable-search" read write)
                  ("expander-column" read write)
                  ("hadjustment" read write)
                  ("headers-clickable" read write)
                  ("headers-visible" read write)
                  ("model" read write)
                  ("reorderable" read write)
                  ("rules-hint" read write)
                  ("search-column" read write)
                  ("vadjustment" read write)
                  )
      (signals ("columns-changed" :columns-changed)
               ("cursor-changed" :cursor-changed)
               ("expand-collapse-cursor-row" :expand-collapse-cursor-row)
               ("move-cursor" :move-cursor)
               ("row-activated" :row-activated)
               ("row-collapsed" :row-collapsed)
               ("row-expanded" :row-expanded)
               ("select-all" :select-all)
               ("select-cursor-parent" :select-cursor-parent)
               ("select-cursor-row" :select-cursor-row)
               ("set-scroll-adjustments" :set-scroll-adjustments)
               ("start-interactive-search" :start-interactive-search)
               ("test-collapse-row" :test-collapse-row)
               ("test-expand-row" :test-expand-row)
               ("toggle-cursor-row" :toggle-cursor-row)
               ("unselect-all" :unselect-all)
               ))

     (tree-store ,<gtk-tree-store> (object)
      (make ,gtk-tree-store-new))
     (tree-view-column ,<gtk-tree-view-column> (widget)
      (make ,(lambda (n t r a e et)
               (letrec
                   ((col (cond ((member "text" a) => cadr) (else 0)))
                    (coercer (type-coercer t))
                    (renderer
                     (if (or e et)
                       (lambda ()
                         (let ((cell (r)))
                           (g-object-set-property cell "editable" #t)
                           (g-signal-connect cell "edited"
                             (or e (lambda (c num text)
                                     (let* ((store (gtk-tree-view-get-model et))
                                            (path (gtk-tree-path-new-from-string num))
                                            (iter (gtk-tree-model-get-iter store path)))
                                       (gtk-tree-store-set-value
                                        store iter col (coercer text))))))
                           cell))
                       r)))
                 (apply gtk-tree-view-column-new-with-attributes
                        n (renderer) a)))
        ((name :default "")
         (type :default ,<string>)
         (renderer :default ,gtk-cell-renderer-text-new)
         (attributes :default ())
         (edited :default #f)
         (edit-tree :default #f)))
      (keywords (,gtk-tree-view-column-set-sort-column-id
                 ,gtk-tree-view-column-get-sort-column-id :sort-column-id)
                (,gtk-tree-view-column-set-sort-indicator
                 ,gtk-tree-view-column-get-sort-indicator :sort-indicator)
                (,gtk-tree-view-column-set-sort-order
                 ,gtk-tree-view-column-get-sort-order :sort-order))
      (properties ("alignment" read write)
                  ("clickable" read write)
                  ("fixed-width" read write)
                  ("max-width" read write)
                  ("min-width" read write)
                  ("reorderable" read write)
                  ("resizable" read write)
                  ("sizing" read write)
                  ("sort-indicator" read write)
                  ("sort-order" read write)
                  ("title" read write)
                  ("visible" read write)
                  ("widget" read write)
                  ("width" read))
      (signals ("clicked" :clicked)))
     (cell-renderer ,<gtk-cell-renderer> (object)
      (make ,gtk-cell-renderer-text-new))

     ;; ranges
     (range ,<gtk-range> (widget))
     (scale ,<gtk-scale> (range))
     (hscale ,<gtk-hscale> (scale)
      (make ,gtk-hscale-new))
     (vscale ,<gtk-vscale> (scale)
      (make ,gtk-vscale-new))
     (scrollbar ,<gtk-scrollbar> (range))
     (hscrollbar ,<gtk-hscrollbar> (scrollbar)
      (make ,gtk-hscrollbar-new))
     (vscrollbar ,<gtk-vscrollbar> (scrollbar)
      (make ,gtk-vscrollbar-new))

     ;; other widgets
     (calendar ,<gtk-calendar> (widget)
      (make ,gtk-calendar-new))
     (drawing-area ,<gtk-drawing-area> (widget)
      (make ,gtk-drawing-area-new))
     (curve ,<gtk-curve> (drawing-area)
      (make ,gtk-curve-new))
     (entry ,<gtk-entry> (widget)
      (make ,gtk-entry-new)
      (keywords (,gtk-entry-set-text #f :text)
                (,gtk-entry-set-activates-default
                 ,gtk-entry-get-activates-default :activates-default))
      (properties ("activates-default" read write)
                  ("cursor-position" read)
                  ("editable" read write)
                  ("has-frame" read write)
                  ("invisible-char" read write)
                  ("max-length" read write)
                  ("scroll-offset" read)
                  ("selection-bound" read)
                  ("text" read write)
                  ("visibility" read write)
                  ("width-chars" read write)
                  )
      (signals ("activate" :activate)
               ("copy-clipboard" :copy-clipboard)
               ("cut-clipboard" :cut-clipboard)
               ("delete-from-cursor" :delete-from-cursor)
               ("insert-at-cursor" :insert-at-cursor)
               ("move-cursor" :move-cursor)
               ("paste-clipboard" :paste-clipboard)
               ("populate-popup" :populate-popup)
               ("toggle-overwrite" :toggle-overwrite)
               )
      )
     (spin-button ,<gtk-spin-button> (entry)
      (make ,gtk-spin-button-new))
     (ruler ,<gtk-ruler> (widget))
     (hruler ,<gtk-hruler> (ruler)
      (make ,gtk-hruler-new))
     (vruler ,<gtk-vruler> (ruler)
      (make ,gtk-vruler-new))
     (separator ,<gtk-separator> (widget))
     (hseparator ,<gtk-hseparator> (separator)
      (make ,gtk-hseparator-new))
     (vseparator ,<gtk-vseparator> (separator)
      (make ,gtk-vseparator-new))
     (invisible ,<gtk-invisible> (widget)
      (make ,gtk-invisible-new))
     (progress-bar ,<gtk-progress-bar> (progress)
      (make ,gtk-progress-bar-new))

     ;; items
     (item ,<gtk-item> (bin))
     (menu-item ,<gtk-menu-item> (item)
      ;;(make ,gtk-menu-item-new-with-label ((label :default "")))
      (make ,gtk-menu-item-new)
      (init
       ,(lambda (obj parent keys)
          (and-let* ((text (get-key :label keys))
                     (maker (if (get-key :mnemonic keys #t)
                              gtk-label-new-with-mnemonic
                              gtk-label-new))
                     (label (maker text)))
            (gtk-widget-show label)
            (gtk-container-add obj label))))
      (append ,gtk-menu-item-set-submenu)
      (signals ("activate" :activate))
      )
     (separator-menu-item ,<gtk-separator-menu-item> (menu-item)
      (make ,gtk-separator-menu-item-new))

     ;; miscellaneous
     (misc ,<gtk-misc> (widget))
     (label ,<gtk-label> (misc)
      (make ,gtk-label-new ((text :default "")))
      (keywords (,gtk-label-set-justify #f :justify)
                (,gtk-label-set-line-wrap #f :line-wrap)
                (,gtk-label-set-pattern #f :pattern)))
     (arrow ,<gtk-arrow> (misc)
      (make ,gtk-arrow-new ((arrow-type :default ,GTK_ARROW_RIGHT)
                            (shadow-type :default ,GTK_SHADOW_IN))))
     (image ,<gtk-image> (misc)
      (make ,gtk-image-new)
      (keywords (,gtk-image-set-from-file #f :file)
                (,gtk-image-set-from-pixmap #f :pixmap)
                (,gtk-image-set-from-pixbuf #f :pixbuf)
                (,gtk-image-set-from-animation #f :animation)
                (,gtk-image-set-from-stock #f :stock)))

     ;; non-widgets
     (proc ,<procedure> (object)
      (make ,(lambda () (lambda () #f))))
     (var ,<object> (object)
      (make ,(lambda () #t)))
     (nil ,<object> (var))

     )))

(provide "uui/object-rules")
