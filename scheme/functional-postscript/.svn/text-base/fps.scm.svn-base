;;;; fps.scm

(module fps
    ( ;; constants
     pi 1/4pi 1/2pi 3/4pi 5/4pi 3/2pi 7/4pi 2pi
     origin
     identity-matrix
     afm-directory-list

     ;; font
     font?
     font

     ;; Point and Matrix
     pt? pt=
     pt pt:x pt:y
     add-pts  negate-pt  scale-pt

     matrix? matrix=
     matrix matrix*

     ;; Path Makers
     path?
     line 
     rect
     arc 
     tangent-arc 
     curve 
     close-path 
     stroke-outline-path 
     bitmap->path
     bounding-box->rect
     the-empty-path

     ;; glyphs construction
     char->glyphpath
     int->glyphpath
     glyphname->glyphpath
     vector->glyphpath
     simple-string->glyphpath
     string->glyphpath

     ;; Picture Makers
     picture?
     stroke fill clip colormap bitmap->pict
     paint-glyphpath
     the-empty-pict

     ;; combination
     compose       compose-path   compose-pict
     join          join-path      join-pict
     link

     ;; transformation
     translate rotate scale

     ;; style
     style?
     vary-default
     build-style	  
     with-style*
     with-style
     (with-attrib default-style)

     ;; attributes
     attrib?
     :color        :line-cap     :line-width 
     :dash-pattern :dash-offset  
     :line-join    :miter-limit

     ;; colors
     color?   color=
     gray     gray:val
     rgb      rgb:r  rgb:g  rgb:b
     hsb      hsb:h  hsb:s  hsb:b
     cmyk     cmyk:c cmyk:m cmyk:y cmyk:k

     ;; char map
     char-map?
     base-char-map
     lookup-char-map
     function->char-map 
     alist->char-map 
     mask-char-map 
     native-font-char-map

     ;; int map
     int-map?
     base-int-map
     lookup-int-map
     function->int-map
     alist->int-map
     mask-int-map
     native-font-int-map

     ;; object info
     start-pt      
     end-pt
     bounding-box  
     bounding-box:max bounding-box:min

     ;; channel
     channel?
     show
     show-w/ps2-text-channel
     ps2-text-channel
     close-channel

     ;; bitmap
     bitmap?
     vector->bitmap
     hex-string->bitmap
     bin-string->bitmap

     ;; options
     :format
     :creator      :creation-date  :title
     :copyright    :for            :routing
     :duplex       :duplex-tumble  :collate  
     :num-copies   :orientation
     :page-label

     ;; util
     deg->rad rad->deg
     inch
     )

  (import scheme 
	  (except chicken define-record))

  (define-syntax :optional
    (syntax-rules ()
      ((_ . more) (optional . more))))

  (define-syntax with-style
    (syntax-rules ()
      ((with-style style exp ...)
       (with-style* style (lambda () exp ...)))))
  
  (define-syntax with-attrib
    (syntax-rules ()
      ((with-attrib (attrib ...) exp ...)
       (with-style (apply build-style (default-style) (list attrib ...))
		   exp ...)))) 

  (require-library extras)
  (import (except extras format))
  (use srfi-1) ;; for filter
  (use srfi-69) ;; for hash-table
  (use regex)   ;; for string-split-fields

  (use records utils format)

  (define (field-splitter rx) 
    (lambda (str #!optional (start 0))
      (string-split-fields rx str #t start) ) )

  (define (infix-splitter rx) 
    (lambda (str #!optional (start 0))
      (string-split-fields rx str #:infix start) ) )

  (define (suffix-splitter rx) 
    (lambda (str #!optional (start 0))
      (string-split-fields rx str #:suffix start) ) )

  (define char->ascii char->integer)
  (define ascii->char integer->char)

  (define (make-string-table) (make-hash-table string=?))
  (define table-ref (cut hash-table-ref/default <> <> #f))
  (define table-set! hash-table-set!)

  (include "conditionals")
  (include "defrec")
  (include "fps.type")
  (include "fps.color")
  (include "fps.util")
  (include "fps-global")
  (include "fps.glyph")
  (include "fps.comp")
  (include "fps.paint")
  (include "fps.map")
  (include "fps.afm")
  (include "fps.ask")
  (include "fps.show")
  (include "fps.mat")
  (include "fps.style")
  (include "fps.bitmap")
  (include "fps.options")
  (include "ps.path")
  (include "ps.misc")

  (define-record-printer (pt p port)
    (format port "#<pt ~s/~s>" (pt:x p) (pt:y p)))

  )