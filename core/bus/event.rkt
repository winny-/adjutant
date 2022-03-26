#lang racket

(provide event%)

(define event%
  (class object%
    (field [created (current-milliseconds)])
    (define/public (get-created) created)
    (super-new)))
