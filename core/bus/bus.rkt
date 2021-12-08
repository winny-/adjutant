#lang curly-fn racket

(require "topic.rkt")

(struct message (topic payload) #:transparent)

(define bus%
  (class object%
    (init-field [subscribers (make-hash)])

    (define/public (subscribe the-topic fn)
      (hash-update! subscribers the-topic #{cons fn %} empty))

    (define/public (publish the-topic payload)
      (define m (message the-topic payload))
      (for ([subscriber (hash-ref subscribers the-topic empty)])
        (match subscriber
          [(? procedure?) (subscriber m)]
          [(? thread?) (thread-send subscriber m)])))

    (super-new)))

(module+ demo
  (define-topic topic:message)
  (define b (new bus%))
  (define (sub payload)
    (printf "S: ~a\n" payload))
  (send b subscribe topic:message sub)
  (send b publish topic:message "Every line will be sent over pub-sub to be echoed back...")
  (let loop ()
    (display "P: ")
    (send b publish topic:message (read-line))
    (loop)))

(module+ demo-thread
  (define-syntax-rule (thread/thunk body ...)
    (thread
     (thunk
      body ...)))
  (define-topic topic:message)
  (define b (new bus%))

  (send b subscribe topic:message
        (thread/thunk
         (let loop ([i 0])
           (match (thread-try-receive)
             [#f (sleep 1)]
             [(struct message (topic payload)) (printf "S: ~a\n" payload)])
           (loop (add1 i)))))

  (thread-wait (thread/thunk
                (send b publish topic:message "Every line will be sent over pub-sub to be echoed back...")
                (let loop ()
                  (display "P: ")
                  (send b publish topic:message (read-line))
                  (loop)))))
