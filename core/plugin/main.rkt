#lang racket

(require racket/rerequire)
(provide (all-defined-out))

(define-signature plugin^
  (description ; string?
   load! ; (-> void?)
   unload! ; (-> void?)
   dependencies))

(struct Plugin [mod description dependencies load! unload!] #:transparent)

(define (get-base mod)
  (match mod
    [(regexp #rx"([^/]+)\\.rkt" (list _ base))
     base]
    [_ #f]))

(define/contract (mod->unit-name mod)
  (-> string? symbol?)
  (string->symbol (format "~a@" (get-base mod))))

(define/contract loaded-plugins
  (hash/c string? Plugin?)
  (make-hash))

(define/contract (find-plugin-mod name)
  (-> string? (or/c string? #f))
  (for*/first ([p (*plugin-path*)]
               [q (in-directory p)]
               #:when (let* ([r (path->string q)] [b (get-base r)]) (and b (string=? name b))))
    (string-replace p (path->string  q) "" #:all? #f)))

(define/contract (load-plugin! mod)
  (-> string? Plugin?)
  (dynamic-rerequire mod #:verbosity 'none)
  (define-values/invoke-unit (dynamic-require mod (mod->unit-name mod))
    (import)
    (export plugin^))
  ;; Load dependencies.
  (for ([dep (in-list dependencies)])
    (load-plugin! (find-plugin-mod dep)))
  ;; Load this plugin.
  (load!)
  ;; Save reference in the plugin table for easy access.
  (define p (Plugin mod description dependencies load! unload!))
  (hash-set! loaded-plugins mod p)
  ;; Return the newly instantiated Plugin.
  p)

(define/contract (stale-plugin? p)
  (-> Plugin? boolean?)
  (not (empty? (dynamic-rerequire (Plugin-mod p) #:verbosity 'none))))

(define/contract (unload-plugin! p)
  (-> Plugin? any/c)
  (begin0
      ((Plugin-unload! p))
    (hash-remove! loaded-plugins (Plugin-mod p))))

(define/contract (reload-plugin! p)
  (-> Plugin? any/c)
  (unload-plugin! p)
  (load-plugin! (Plugin-mod p)))

(define/contract (load-directory! dir)
  (-> path-string? (listof Plugin?))
  (for/list ([p (in-directory dir)])
    ;; XXX
    (load-plugin! p)))

(define *plugin-path* (make-parameter (list (path->string (current-directory)))
                                      (listof string?)))
(define/contract (load-path!)
  (-> (listof Plugin?))
  (flatten (for/list ([p (in-list (*plugin-path*))])
             (load-directory! p))))

(module+ main
  (define targets '("plugins/hello.rkt"))
  (displayln "Starting up...")
  (for ([p targets])
    (load-plugin! p))
  (printf "loaded plugins: ~v\n" (hash-keys loaded-plugins))
  (displayln "Shutting down...")
  (for ([p (in-hash-values (hash-copy loaded-plugins))])
    (unload-plugin! p)))
