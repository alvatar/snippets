;;;; ftp.scm - Simple FTP client operations - felix


(module ftp (ftp:connect 
	     ftp:disconnect ftp:set-type! ftp:change-directory ftp:open-input-file ftp:open-output-file 
	     ftp:open-list ftp:abort ftp:ftp? ftp:delete-file ftp:rename-file ftp:set-mode!
	     ftp:delete-directory ftp:create-directory)
  (import scheme chicken)
  (use tcp extras regex matchable ports)

(define-constant default-port-number 21)

(define-record ftp-connection
  verbose				; bool
  port					; #f | port
  passive?				; bool
  in					; port
  out)					; port

(define ftp:ftp? ftp-connection?)

(define (dribble fstr . args)
  (fprintf (current-error-port) "FTP: ~?~%" fstr args) )

(define (ftp-error msg code)
  (signal
   (make-composite-condition 
    (make-property-condition 'exn 'message msg)
    (make-property-condition 'ftp 'code code) ) ) )

(define (ftp:connect host user pass . more)
  (let-optionals more ([v #f] [port default-port-number])
    (when v (dribble "Connecting to ~A at port ~A ..." host port))
    (let-values ([(i o) (tcp-connect host port)])
      (let ([ftp (make-ftp-connection v #f #t i o)])
	(get-reply ftp)
	(login ftp user pass)
	ftp) ) ) )

(define (get-reply ftp)
  (let loop ()
    (let ([ln (read-line (ftp-connection-in ftp))])
      (when (eof-object? ln)
	(error "unexpected end of file in FTP reply" ftp) )
      (when (ftp-connection-verbose ftp) (dribble "[~A]" ln))
      (match (string-match "([0-9]{3})(\\-| )(.*)" ln)
	[(_ code cont msg)
	 (if (string=? "-" cont)
	     (loop)
	     (let ([ncode (string->number code)])
	       (if (>= ncode 400)
		   (ftp-error msg ncode)
		   (cons ncode msg) ) ) ) ]
	[_ (ftp-error (string-append "invalid server reply: " ln) 0)] ) ) ) )

(define (login ftp user passwd)
  (send-command ftp (sprintf "USER ~A" user))
  (send-command ftp (sprintf "PASS ~A" passwd)) )

(define (ftp:disconnect ftp)
  (when (ftp-connection-verbose ftp) (dribble "Closing connection..."))
  (send-command ftp "QUIT") )

(define (send-command ftp str)
  (when (ftp-connection-verbose ftp) (fprintf (current-error-port) "FTP: ~A~%" str))
  (let ([out (ftp-connection-out ftp)])
    (display str out)
    (display "\r\n" out)
    (get-reply ftp) ) )

(define (ftp:set-mode! ftp m)
  (ftp-connection-passive?-set! 
   ftp 
   (case m
     [(active) #f]
     [(passive) #t]
     [else (ftp-error (sprintf "invalid mode argument ~S" m) 0)] ) ) )

(define (ftp:set-type! ftp m)
  (send-command
   ftp
   (sprintf 
    "TYPE ~A"
    (case m
      [(binary image) #\I]
      [(ascii) #\A]
      [else (ftp-error (sprintf "invalid type argument ~S" m) 0)] ) ) ) )

(define (ftp:change-directory ftp dir)
  (send-command ftp (sprintf "CWD ~A" dir)) )

(define (ftp:open-list ftp . more)
  (when (ftp-connection-port ftp)
    (ftp-error "FTP transfer already in progress" 0) )
  (let-optionals more ([mask #f] [long #f])
    (let ([io (send-port-command ftp)])
      (ftp-connection-port-set! ftp #t)
      (send-command
       ftp
       (cond [mask (sprintf "LIST ~A" mask)]
	     [long "LIST"]
	     [else "NLST"] ) )
      (let-values ([(i o) (ports-from-data-port io)])
	(let ([port 
	       (make-input-port
		(lambda () (read-char i))
		(lambda () (char-ready? i))
		(lambda ()
		  (close-input-port i)
		  (close-output-port o)
		  (close-data-port ftp io)
		  (ftp-connection-port-set! ftp #f) ) ) ] )
	  (ftp-connection-port-set! ftp port)
	  port) ) ) ) )

(define (send-port-command ftp)
  (if (ftp-connection-passive? ftp)
      (match (string-search 
	      " \\(([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+)\\)\\." 
	      (cdr (send-command ftp "PASV")) )
	[(_ ip1 ip2 ip3 ip4 p1 p2)
	 (let-values ([(i o) 
		       (tcp-connect 
			(sprintf "~A.~A.~A.~A" ip1 ip2 ip3 ip4)
			(bitwise-ior (arithmetic-shift (string->number p1) 8) (string->number p2)) ) ] )
	   (cons i o) ) ]
	[_ (ftp-error "invalid response to PASV command from FTP server" 0)] )
      (let* ([listener (tcp-listen 0 1)]
	     [port (tcp-listener-port listener)] )
	(let-values ([(me _) (tcp-addresses (ftp-connection-in ftp))])
	  (match (string-match "([0-9]+)\\.([0-9]+)\\.([0-9]+)\\.([0-9]+)" me)
	    [(_ ip1 ip2 ip3 ip4)
	     (send-command
	      ftp
	      (sprintf "PORT ~A,~A,~A,~A,~A,~A" ip1 ip2 ip3 ip4 (arithmetic-shift port -8) (bitwise-and #xff port)) )
	     listener] 
	    [_ (ftp-error "invalid response to PORT command from FTP server" 0)] ) ) ) ) )

(define (ports-from-data-port x)
  (match x
    [(i . o) (values i o)]
    [(? tcp-listener?) (tcp-accept x)] ) )

(define (close-data-port ftp x)
  (if (tcp-listener? x)
      (tcp-close x)
      (get-reply ftp) ) )

(define (ftp:open-input-file ftp fname)
  (when (ftp-connection-port ftp)
    (ftp-error "FTP transfer already in progress" 0) )
  (let ([io (send-port-command ftp)])
    (ftp-connection-port-set! ftp #t)
    (send-command ftp (sprintf "RETR ~A" fname))
    (let-values ([(i o) (ports-from-data-port io)])
      (close-output-port o)
      (let ([port
	     (make-input-port
	      (lambda () (read-char i))
	      (lambda () (char-ready? i))
	      (lambda () 
		(close-input-port i)
		(close-data-port ftp io)
		(ftp-connection-port-set! ftp #f) ) ) ] )
	(ftp-connection-port-set! ftp port)
	port) ) ) )

(define (ftp:open-output-file ftp fname)
  (when (ftp-connection-port ftp)
    (ftp-error "FTP transfer already in progress" 0) )
  (let ([io (send-port-command ftp)])
    (ftp-connection-port-set! ftp #t)
    (send-command ftp (sprintf "STOR ~A" fname))
    (let-values ([(i o) (ports-from-data-port io)])
      (close-input-port i)
      (let ([port
	     (make-output-port
	      (lambda (s) (display s o))
	      (lambda () 
		(close-output-port o)
		(close-data-port ftp io)
		(ftp-connection-port-set! ftp #f) ) ) ] )
	(ftp-connection-port-set! ftp port)
	port) ) ) )

(define (ftp:abort ftp)
  (let ([p (ftp-connection-port ftp)])
    (when p
      (send-command ftp "ABOR")
      ((if (input-port? p) close-input-port close-output-port) p)
      (ftp-connection-port-set! ftp #f) ) ) )

(define (ftp:delete-file ftp fname)
  (send-command ftp (sprintf "DELETE ~A" fname)) )

(define (ftp:rename-file ftp old new)
  (send-command ftp (sprintf "RNFR ~A" old))
  (send-command ftp (sprintf "RNTO ~A" new)) )

(define (ftp:delete-directory ftp dir)
  (send-command ftp (sprintf "RMD ~A" dir)) )

(define (ftp:create-directory ftp dir)
  (send-command ftp (sprintf "MKD ~A" dir)) )

)
