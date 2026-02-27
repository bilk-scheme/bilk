;; dns-lookup.scm — Resolve hostnames using (bilk net) DNS primitives.
;;
;; Run:
;;   bilk run examples/dns-lookup.scm

(import (bilk net))

(define (lookup host)
  (display host)
  (display " -> ")
  (display (net-resolve host))
  (newline))

(define (lookup-all host)
  (display host)
  (display " -> ")
  (display (net-resolve-all host))
  (newline))

(display "--- net-resolve (first result) ---")
(newline)
(lookup "localhost")
(lookup "example.com")

(newline)
(display "--- net-resolve-all (all results) ---")
(newline)
(lookup-all "localhost")
(lookup-all "example.com")
