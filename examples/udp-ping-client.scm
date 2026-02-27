;; udp-ping-client.scm — Client for udp-ping-server.scm.
;;
;; Start the server first, then run this client with the port it prints:
;;   bilk run examples/udp-ping-server.scm
;;   bilk run examples/udp-ping-client.scm <port>

(import (bilk net))

(define args (command-line))
(when (< (length args) 2)
  (display "Usage: bilk run examples/udp-ping-client.scm <port>")
  (newline)
  (exit 1))

(define server-port (string->number (list-ref args 1)))

(define sock (udp-open))
(udp-bind! sock "127.0.0.1" 0)

;; Send three pings and read the pong replies.
(let loop ((n 3))
  (when (> n 0)
    (display "Sending ping...")
    (newline)
    (udp-send-to sock
                 (string->utf8 "ping")
                 "127.0.0.1"
                 server-port)
    (let* ((packet (udp-receive sock 1024))
           (data   (utf8->string (car packet))))
      (display "Received: ")
      (display data)
      (newline))
    (loop (- n 1))))

;; Tell the server to shut down.
(udp-send-to sock
             (string->utf8 "quit")
             "127.0.0.1"
             server-port)
(display "Done.")
(newline)

(udp-close sock)
