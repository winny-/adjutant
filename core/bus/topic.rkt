#lang racket

(provide (all-defined-out))

(struct topic () #:transparent)

(define-syntax-rule (define-topic the-topic)
  (struct the-topic topic () #:transparent))
