#lang racket

(require "event.rkt"
         "topic.rkt"
         "bus.rkt")

(define readline-event%
  (class event%
    (init-field line)
    (field [semaphore (make-semaphore)])
    (define/public (get-line) line)
    (define/public (wait) (semaphore-wait semaphore))
    (define/public (post) (semaphore-post semaphore))
    (define/public (sync thunk)
      (call-with-semaphore semaphore thunk))
    (super-new)))

(define (make-readline-event s)
  (new readline-event% [line s]))

(module+ demo
  (define-topic topic:message)
  (define b (new bus%))
  (define (sub message)
    (match-define (struct* Message ([event event])) message)
    (printf "S ~a: ~a\n" (send event get-created) (send event get-line)))
  (send b subscribe topic:message sub)
  (send b publish topic:message (make-readline-event "Every line will be sent over pub-sub to be echoed back..."))
  (let loop ()
    (display "P: ")
    (send b publish topic:message (make-readline-event (read-line)))
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
             [(struct Message (topic event))
              (printf "S ~a: ~a\n" (send event get-created) (send event get-line))
              (send event post)])
           (loop (add1 i)))))

  (thread-wait (thread/thunk
                (define e1 (make-readline-event "Every line will be sent over pub-sub to be echoed back..."))
                (send b publish topic:message e1)
                (send e1 wait)
                (let loop ()
                  (display "P: ")
                  (define e (make-readline-event (read-line)))
                  (send b publish topic:message e)
                  (send e wait)
                  (loop)))))
