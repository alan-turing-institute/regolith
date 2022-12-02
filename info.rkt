#lang info
(define collection "regolith")
(define deps '("base" "gregor"
                      "git+https://github.com/alan-turing-institute/nocell.git"
                      "git+https://github.com/alan-turing-institute/whatnow.git"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/regolith.scrbl" ())))
(define pkg-desc "Reporting from REG")
(define version "0.0.1")
(define pkg-authors '(ostrickson@turing.ac.uk jgeddes@turing.ac.uk))
(define license '(MIT))
