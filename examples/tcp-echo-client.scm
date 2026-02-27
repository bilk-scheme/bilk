;; tcp-echo-client.scm — Client for tcp-echo-server.scm.
;;
;; Start the server first, then run this client with the port it prints:
;;   bilk run examples/tcp-echo-server.scm
;;   bilk run examples/tcp-echo-client.scm <port>

(import (bilk net))

(define args (command-line))
(when (< (length args) 2)
  (display "Usage: bilk run examples/tcp-echo-client.scm <port>")
  (newline)
  (exit 1))

(define port (string->number (list-ref args 1)))
(define conn (tcp-connect "127.0.0.1" port))
(define in  (car conn))
(define out (cdr conn))

(define messages '("hello world" "bilk scheme" "networking is fun"))

(for-each
  (lambda (msg)
    (display "Sent:     ")
    (display msg)
    (newline)
    (write-string msg out)
    (write-string "\n" out)
    (flush-output-port out)
    (let ((reply (read-line in)))
      (display "Received: ")
      (display reply)
      (newline)))
  messages)

(close-input-port in)
(close-output-port out)
