#lang curly-fn racket

(require "topic.rkt"
         "event.rkt")

(provide (all-defined-out))

(struct Message (topic event) #:transparent)

(define bus%
  (class object%
    (init-field [subscribers (make-hash)])

    (define/public (subscribe the-topic fn)
      (hash-update! subscribers the-topic #{cons fn %} empty))

    (define/public (publish the-topic event)
      (define m (Message the-topic event))
      (for ([subscriber (hash-ref subscribers the-topic empty)])
        (match subscriber
          [(? procedure?) (subscriber m)]
          [(? thread?) (thread-send subscriber m)])))

    (super-new)))
