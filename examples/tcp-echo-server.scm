;; tcp-echo-server.scm — TCP echo server that uppercases input.
;;
;; Run the server, then connect with its companion client:
;;   bilk run examples/tcp-echo-server.scm
;;   bilk run examples/tcp-echo-client.scm <port>

(import (bilk net)
        (scheme char))

(define server (tcp-listen 0))
(define port (tcp-listener-port server))

(display "Echo server listening on port ")
(display port)
(newline)
(flush-output-port (current-output-port))

(define conn (tcp-accept server))
(define in  (car conn))
(define out (cdr conn))

(let loop ()
  (let ((line (read-line in)))
    (unless (eof-object? line)
      (write-string (string-upcase line) out)
      (write-string "\n" out)
      (flush-output-port out)
      (loop))))

(close-input-port in)
(close-output-port out)
(tcp-close server)
(display "Server shut down.")
(newline)
