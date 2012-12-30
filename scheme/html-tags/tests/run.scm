#!/usr/bin/csi -script

(use test html-tags)

(xhtml-style? #t)

(test "<hr />" (<hr>))
(test "<br />" (<br>))
(test "<img src='pic.png' alt='nothing' />" (<img> src: "pic.png" alt: "nothing"))

(xhtml-style? #f)

(test "<a href='ali'>opa</a>" (<a> href: "ali" "opa"))
(test "<pre>a</pre>" (<pre> "a"))
(test "<div id='some'><img src='pic.jpg'></div>" (<div> id: "some" (<img> src: "pic.jpg")))
(test "<div><p>a</p><p>b</p></div>" (<div> (<p> "a") (<p> "b")))
(test "<p align=\"center\">aloalo</p>" (<p> align: "center" quote-procedure: (lambda (x) (conc "\"" x "\"")) "alo" "alo"))
(test "<select><option value='val' selected>opt</option></select>" (<select> (<option> value: "val" selected: #t "opt")))
(test "<select><option value='val'>opt</option></select>" (<select> (<option> value: "val" selected: #f "opt")))
(test "<p>&lt;p&gt;hello&lt;/p&gt;</p>" (<p> convert-to-entities?: #t (<p> "hello")))
(test "<section>foo</section>" (<section> "foo"))
(test "<section data-type='foo'>bar</section>" (<section> data-type: "foo" "bar"))

(generate-sxml? #t)

(test '(html) (<html>))
(test '(p (@ (align "center"))) (<p> align: "center"))
(test '(a (@ (href "ali")) "opa") (<a> href: "ali" "opa"))
(test '(pre "a") (<pre> "a"))
(test '(div (@ (id "some")) (img (@ (src "pic.jpg")))) (<div> id: "some" (<img> src: "pic.jpg")))
(test '(div (p "a") (p "b")) (<div> (<p> "a") (<p> "b")))
(test '(p (@ (align "center")) "alo" "alo") (<p> align: "center"  "alo" "alo"))
(test '(select (option (@ (value "val") (selected)) "opt")) (<select> (<option> value: "val" selected: #t "opt")))
(test '(select (option (@ (value "val")) "opt")) (<select> (<option> value: "val" selected: #f "opt")))
(test '(p foo) (<p> 'foo))
(test '(section "foo") (<section> "foo"))
(test '(section (@ (data-type "foo")) "bar")  (<section> data-type: "foo" "bar"))
(test-exit)
