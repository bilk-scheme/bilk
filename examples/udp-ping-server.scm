;; udp-ping-server.scm — UDP server that replies "pong" to every "ping".
;;
;; Start the server, then run its companion client:
;;   bilk run examples/udp-ping-server.scm
;;   bilk run examples/udp-ping-client.scm <port>

(import (bilk net))

(define sock (udp-open))
(udp-bind! sock "127.0.0.1" 0)

(define addr (udp-local-address sock))
(display "UDP ping server listening on 127.0.0.1:")
(display (cdr addr))
(newline)
(flush-output-port (current-output-port))

(let loop ()
  (let* ((packet (udp-receive sock 1024))
         (data   (utf8->string (car packet)))
         (sender (cdr packet)))
    (display "Received: ")
    (display data)
    (display " from ")
    (display (car sender))
    (display ":")
    (display (cdr sender))
    (newline)
    (cond
      ((string=? data "quit")
       (display "Shutting down.")
       (newline))
      (else
       (udp-send-to sock
                    (string->utf8 "pong")
                    (car sender)
                    (cdr sender))
       (loop)))))

(udp-close sock)
