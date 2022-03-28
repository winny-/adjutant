#lang racket

(require "../core/bus/main.rkt"
         irc-client
         (only-in irc irc-message)
         dotenv)

(dotenv-load!)

(define NAME (getenv "IRC_NICK"))
(define PASSWORD (getenv "IRC_PASSWORD"))

(define-values (conn ready-evt)
  (irc-connect (getenv "IRC_SERVER") 6697 NAME NAME NAME #:ssl 'auto))

(sync ready-evt)

(sleep 2)
(irc-send-message! conn "nickserv" (string-append "identify " PASSWORD))
(sleep 2)

(displayln "joining...")

(irc-join-channel! conn (getenv "IRC_CHANNEL"))

(displayln "joined.")

(define bus (new bus%))

(define nick NAME)
(define (nick-watcher m)
  (match m
    [(struct Message ['irc o])
     (match (send o get-irc-message)
       [(struct* IrcMessage-Nick ([user (struct* IrcUser ([nick (== nick)]))]
                                  [new-nick new-nick]))
        (set! nick NAME)]
       [_ (void)])]))

(define irc-event%
  (class event%
    (init-field irc-message
                irc-connection)
    (define/public (get-irc-message)
      irc-message)
    (super-new)))

(define irc-message-event%
  (class irc-event%
    (inherit-field irc-message
                   irc-connection)
    (define/public (get-message)
      (IrcMessage-Message-content irc-message))
    (define/public (get-sender)
      (IrcUser-nick (IrcMessage-Message-sender irc-message)))
    (define/public (reply s)
      (define target (IrcMessage-Message-recipient irc-message))
      (printf "PRIVMSG ~a: <~a> ~a\n" target nick s)
      (irc-send-message! irc-connection target s))
    (super-new)))

(define (.bots m)
  (match-define (struct Message ['irc:privmsg o]) m)
  (when (string=? (send o get-message) ".bots")
    (send o reply "Adjutant Bus demo bot [Racket] https://github.com/winny-/adjutant")))

(define (.nick m)
  (match-define (struct Message ['irc:privmsg o]) m)
  (and (string=? (getenv "IRC_ADMIN") (send o get-sender))
       (match (string-split (send o get-message))
         [(list ".nick" new-nick)
          (irc-set-nick! conn new-nick)
          (displayln (format "NICK ~a -> ~a" nick new-nick))
          (set! nick new-nick)]
         [_ (void)])))

(define (log-activity m)
  (let/ec esc
    (match-define (struct Message [_ o]) m)
    (define msg (send o get-irc-message))
    (displayln
     (match (send o get-irc-message)
       [(and o (struct* IrcMessage-Message ([sender (struct* IrcUser ([nick nick]))] [recipient recipient] [content content])))
        (if (IrcMessage-ChatMessage? o)
            (format "PRIVMSG ~a: <~a> ~a" recipient nick content)
            (format "ACTION ~a: <~a> ~a" recipient nick content))]
       [(struct* IrcMessage-Notice ([sender (struct* IrcUser ([nick nick]))] [recipient recipient] [content content]))
        (format "NOTICE ~a: <~a> ~a" recipient nick content)]
       [(struct* IrcMessage-Join ([user (struct* IrcUser ([nick nick]))] [channel channel]))
        (format "JOIN ~a: ~a" channel nick)]
       [(struct* IrcMessage-Part ([user (struct* IrcUser ([nick nick]))] [channel channel] [reason reason]))
        (format "PART ~a: ~a (~a)" channel nick reason)]
       [(struct* IrcMessage-Quit ([user (struct* IrcUser ([nick nick]))] [reason reason]))
        (format "QUIT ~a: ~a" nick reason)]
       [(struct* IrcMessage-Nick ([user (struct* IrcUser ([nick nick]))] [new-nick new-nick]))
        (format "NICK ~a -> ~a" nick new-nick)]
       [(struct* IrcMessage-Kick ([user (struct* IrcUser ([nick nick]))] [kicked-user kicked-user] [channel channel] [reason reason]))
        (format "KICK ~a: ~a by ~a (~a)" channel kicked-user nick reason)]
       [(struct* IrcMessage-Kill ([user (struct* IrcUser ([nick nick]))] [killed-user killed-user] [reason reason]))
        (format "KILL ~a by ~a (~a)" killed-user nick reason)]
       [(struct* IrcMessage ([internal-message internal-message]))
        (match internal-message
          [(struct* irc-message ([command "PING"]))
           (esc)]                 ; Do not log PINGs.
          [(struct* irc-message ([prefix prefix] [command command] [parameters parameters] [content content]))
           (format "~v ~v ~v ~v"
                   prefix
                   #;                          (string-trim (string-append prefix " "))
                   command
                   parameters
                   #;
                   (string-join
                    parameters
                    "; "
                    #:before-first " ["
                    #:after-last "] ")
                   content)])]
       [other (format "??? ~a ???" other)]))))

(send bus subscribe 'irc nick-watcher)
(send bus subscribe 'irc log-activity)

(send bus subscribe 'irc:privmsg .bots)
(send bus subscribe 'irc:privmsg .nick)

(let loop ()
  (match (irc-recv! conn)
    [(? IrcMessage-ChatMessage? msg)
     (define e (new irc-message-event% [irc-message msg] [irc-connection conn]))
     (send bus publish 'irc:privmsg e)
     (send bus publish 'irc e)]
    [other (send bus publish 'irc (new irc-event% [irc-message other] [irc-connection conn]))])
  (loop))
