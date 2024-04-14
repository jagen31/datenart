#lang info

(define version "0.0.0")
(define collection "datenart")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "scribble-math"
                     "racket-doc"
                     "datenart-lib"))
(define scribblings '(("scribblings/datenart.scrbl" (multi-page) (language))))
(define clean '("compiled" "doc" "doc/datenart"))
