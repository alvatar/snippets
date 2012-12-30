(module html-tags

( ;; procedures
<a> <abbr> <acronym> <address> <applet> <area> <b> <base> <basefont>
<bdo> <big> <blink> <blockquote> <body> <bold> <br> <button> <caption>
<center> <cite> <code> <colgroup> <dd> <del> <dir> <div> <dfn> <dl>
<dt> <em> <embed> <fieldset> <font> <form> <frame> <frameset> <h1>
<h2> <h3> <h4> <h5> <h6> <head> <html> <hr> <i> <iframe> <img> <input>
<ins> <kbd> <label> <legend> <li> <link> <map> <menu> <meta> <noembed>
<noframes> <noscript> <object> <option> <optgroup> <ol> <p> <param>
<pre> <q> <script> <s> <samp> <select> <small> <span> <strong> <sub>
<sup> <strike> <style> <table> <td> <textarea> <thead> <tbody> <tfoot>
<th> <title> <tr> <tt> <u> <ul> <var> <!--

;; html5-related procedures
<article> <aside> <audio> <canvas> <command> <datagrid> <datalist>
<datatemplate> <details> <dialog> <embed> <eventsource> <figcaption>
<figure> <footer> <header> <hgroup> <keygen> <mark> <meter> <nav>
<nest> <output> <progress> <rp> <rt> <ruby> <rule> <section> <source>
<summary> <time> <video> <wbr>

;; parameters
xhtml-style? check-html-syntax? generate-sxml?)

(import scheme chicken srfi-1 srfi-13 data-structures)
(use utils)

(define xhtml-style? (make-parameter #f))

(define-for-syntax tags/attribs
  (let ((common-attribs
         '(quote-procedure: convert-to-entities?:
           id: class: lang: title: style: dir: lang: xml:lang: tabindex:
           accesskey: onabort: onblur: onchange: onclick: ondblclick:
           onfocus: onkeydown: onkeypress: onkeyup: onload: onmousedown:
           onmousemove: onmouseover: onmouseout: onmouseup: onreset:
           onselect: onsubmit: onunload:
           )))
    (map (lambda (tags/attribs)
           (append tags/attribs common-attribs))
         '((a            name: href: hreflang: type: rel: rev: charset: coords: shape: target:)
           (abbr         )
           (acronym      )
           (address      )
           (applet       )
           (area         alt: coords: hash: host: hostname: href: noHref: pathname:
                         port: protocol: search: shape: target:)
           (article      ) ; html5
           (aside        ) ; html5
           (audio        ) ; html5
           (b            )
           (base         href: target:)
           (basefont     )
           (bdo          )
           (big          )
           (blink        ) ;; attributes?
           (blockquote   )
           (body         background: bgcolor: text: link: vlink: alink: aLink: scrollleft: scrolltop:)
           (bold         )
           (br           clear:)
           (button       disabled: form: name: type: value:)
           (caption      )
           (canvas       ) ; html5
           (center       )
           (cite         )
           (code         )
           (colgroup     )
           (command      ) ; html5
           (datagrid     ) ; html5
           (datalist     ) ; html5
           (datatemplate ) ; html5
           (dd           )
           (del          )
           (details      ) ; html5
           (dialog       ) ; html5
           (dir          )
           (div          )
           (dfn          )
           (dl           )
           (dt           )
           (em           )
           (embed        src: width: height: align: name: pluginspage: pluginurl: hidden: href: target: units:
                         autostart: loop: playcount: volume: controls: controller: mastersound: starttime: endtime:)
           (eventsource  ) ; html5
           (fieldset     )
           (figcaption   ) ; html5
           (figure       ) ; html5
           (font         color: face: size:)
           (form         action: method: acceptcharset: encoding: enctype: length: name: target:)
           (footer       ) ; html5
           (frame        src: contentdocument: frameborder: longdesc: marginheight: marginwidth: name: noresize: scrolling:)
           (frameset     rows: cols:)
           (h1           align:)
           (h2           align:)
           (h3           align:)
           (h4           align:)
           (h5           align:)
           (h6           align:)
           (head         )
           (header       ) ; html5
           (hgroup       ) ; html5
           (html         )
           (hr           align:)
           (i            )
           (iframe       src: width: align: height: contentdocument: frameborder: longdesc:
                         marginheight: marginwidth: name: noresize: scrolling:)
           (img          src: alt: align: height: width: border: hspace: vspace: usemap: ismap: longdesc: lowsrc:)
           (input        type: name: value: size: maxlength: checked: src: accept:
                         align: alt: defaultchecked: disabled: form:)
           (ins          )
           (keygen       ) ; html5
           (kbd          )
           (label        for: onfocus: onblur:)
           (legend       )
           (li           type: value:)
           (link         charset: disabled: href: hreflang: media: name: rev: rel: target: type:)
           (map          )
           (mark         ) ; html5
           (menu         )
           (meta         name: content: charset: disabled: http-equiv: scheme:)
           (meter        ) ; html5
           (nav          ) ; html5
           (nest         ) ; html5
           (noembed      )
           (noframes     )
           (noscript     )
           (object       )
           (ol           )
           (option       value: defaultselected: disabled: form: index: label: selected: text:)
           (optgroup     label: disabled:)
           (output       ) ; html5
           (p            align:) ;; something else?
           (param        )
           (pre          width:)
           (progress     ) ; html5
           (q            )
           (rp           ) ; html5
           (rt           ) ; html5
           (ruby         ) ; html5
           (rule         ) ; html5
           (s            )
           (samp         )
           (script       src: type: language:)
           (section      ) ; html5
           (select       name: align: disabled: form: length: multiple: selectedindex: size: type: value:)
           (small        )
           (source       ) ; html5
           (span         )
           (strong       )
           (sub          )
           (summary      ) ; html5
           (sup          )
           (strike       )
           (style        media: type:)
           (table        align: border: cellspacing: cellpadding: color: frame: rules: summary: valign: width: bgcolor:)
           (td           rowspan: colspan: nowrap: align: valign: width: height: abbr: axis: background:
                         bgcolor: bordercolor: cellindex: ch: choff: disabled: headers: innerhtml: innertext:
                         rowspan: scope:)
           (textarea     name: rows: cols: wrap: defaultvalue: disabled: readonly: form:)
           (thead        )
           (tbody        )
           (tfoot        )
           (th           rowspan: colspan: nowrap: align: valign: width: height: abbr: axis: background:
                         bgcolor: bordercolor: cellindex: ch: choff: disabled: headers: innerhtml: innertext:
                         rowSpan: scope:)
           (time         ) ; html5
           (title        )
           (tr           align: valign: bgcolor: rowspan: colspan: nowrap: align: valign: width: height: abbr:
                         axis: background: bgcolor: bordercolor: rowindex: ch: choff: disabled: headers:
                         innerhtml: innertext: scope: sectionrowindex: outerhtml: outertext:)
           (tt           )
           (u            bgcolor:)
           (ul           type compact:)
           (var          )
           (video        ) ; html5
           (wbr          ) ; html5
           ))))

(define open-only-tags (map symbol->string '(base br col embed hr img input link meta param)))

(define check-html-syntax? (make-parameter #f))

(define generate-sxml? (make-parameter #f))

(define-syntax make-tag
  (lambda (exp r cmp)
    (let ((tag (cadr exp)))
      `(,(r 'define) ,(string->symbol (string-append "<" (symbol->string tag) ">"))
        (,(r 'lambda) attribs
         (if (generate-sxml?)

             ;; SXML generation
             (let ((sxml-attrs/vals '())
                   (attrs/vals (chop attribs 2))
                   (contents '())
                   (keyword->symbol
                    (lambda (k)
                      (string->symbol (string-chomp (symbol->string k) ":")))))
               (for-each (lambda (attr/val)
                           (unless (null? attr/val)
                             (let ((attr (car attr/val))
                                   (val (cdr attr/val)))
                               (if (keyword? attr)
                                   (unless (null? val)
                                     (let* ((val (car val))
                                            (boolean-val? (boolean? val)))
                                       (if boolean-val?
                                           (when val
                                             (set! sxml-attrs/vals
                                                   (append sxml-attrs/vals
                                                           (list (list (keyword->symbol attr))))))
                                           (set! sxml-attrs/vals
                                                 (append sxml-attrs/vals
                                                         (list (list (keyword->symbol attr)
                                                                     (->string val))))))))
                                   (set! contents
                                         (append contents
                                                 (list attr)
                                                 (if (null? val)
                                                     '()
                                                     (list (car val)))))))))
                         attrs/vals)
               (append (list ',tag)
                       (if (null? sxml-attrs/vals)
                           '()
                           (list (cons '@ sxml-attrs/vals)))
                       contents))

             ;; Strings generation
             ,(let ((tag (->string tag)))
                `(let ((tag-attribs (quote ,(alist-ref (string->symbol tag) tags/attribs)))
                       (check-syntax (check-html-syntax?))
                       (warnings '())
                       (tag-text (string-append "<" ,tag))
                       (attrs/vals (chop attribs 2))
                       (contents "")
                       (open-only (member ,tag open-only-tags))
                       (quote-proc (or (get-keyword 'quote-procedure: attribs)
                                       (lambda (text) (string-append "'" text "'"))))
                       (convert-to-entities? (get-keyword 'convert-to-entities?: attribs))
                       (htmlize (lambda (str) ;; stolen from spiffy
                                  (string-translate* str '(("<" . "&lt;")    (">" . "&gt;")
                                                           ("\"" . "&quot;") ("'" . "&#x27;") ("&" . "&amp;"))))))
                   (for-each (lambda (attr/val)
                               (unless (null? attr/val)
                                 (let ((attr (car attr/val))
                                       (val (cdr attr/val)))
                                   (if (keyword? attr)
                                       (begin
                                         (when (and check-syntax
                                                    (not (memq attr tag-attribs))
                                                    (not (string-prefix? "data-" (->string attr))))
                                           (set! warnings (cons attr warnings)))
                                         (unless (memq attr '(quote-procedure: convert-to-entities?:))
                                           (unless (null? val)
                                             (let* ((val (car val))
                                                    (boolean-val? (boolean? val)))
                                               (if boolean-val?
                                                   (when val
                                                     (set! tag-text (string-append tag-text " " (keyword->string attr))))
                                                   (set! tag-text
                                                         (string-append tag-text
                                                                        " "
                                                                        (keyword->string attr)
                                                                        "="
                                                                        (quote-proc (->string val)))))))))
                                       (set! contents (string-append contents (->string attr)
                                                                     (if (null? val)
                                                                         ""
                                                                         (->string (car val)))))))))
                             attrs/vals)
                   (set! tag-text (string-append tag-text
                                                 (if (and open-only (xhtml-style?))
                                                     " />"
                                                     ">")))
                   (string-append (if (null? warnings)
                                      ""
                                      (string-append "<!-- WARNING: (<" ,tag ">): invalid attributes: "
                                                     (string-intersperse (map ->string warnings)) " -->"))
                                  tag-text
                                  (if convert-to-entities?
                                      (htmlize contents)
                                      contents)
                                  (if open-only
                                      ""
                                      (string-append "</" ,tag ">")))))))))))

(define (<!-- . comments)
  (string-append "<!-- " (string-intersperse (map ->string comments)) " -->"))

(define-syntax make-tags
  (lambda (exp r cmp)
    `(begin
       ,@(map (lambda (tag)
                `(make-tag ,tag))
              (map car tags/attribs)))))

(make-tags tags)

) ; end module
