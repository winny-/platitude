#lang info
(define collection "platitude")
(define version "0.0.1")
(define deps
  '("base"
    "db-lib"
    "web-server-lib"
    "mime-type-lib"
    "rackunit-lib"
    "rackunit-typed"
    "typed-racket-lib"
    "binaryio-lib"))
(define racket-launcher-names '("platitude"))
(define racket-launcher-libraries '("main.rkt"))
