;; -*- mode: scheme; compile-command: "make run" -*-

(import
 (owl toplevel)
 (owl variable)
 (owl json)
 (prefix (owl sys) sys/)
 (robusta encoding base64)
 (prefix (robusta encoding json) json/)
 (prefix (tls) tls/))

(define token (read (open-input-file "token")))

(define (error-out str)
  (catch-thread 'heartbeat)
  (catch-thread 'auth)
  (print "exiting with error '" str "'")
  (exit-owl 1))

(define (force-read c n)
  (let loop ((n n) (acc '()))
    (if (= n 0)
        acc
        (let* ((l (tls/read c n)))
          (sleep 10)
          (loop (- n (length l))
                (append acc l))))))

(define mask '(21 37 42 11)) ;; YEAHH
(define (mask-payload payload mask)
  (map
   (λ (n) (bxor (list-ref payload n)
                (list-ref mask (modulo n 4))))
   (iota 0 1 (length payload))))

(define (send-message chan-id message)
  (let* ((con (open-connection (sys/resolve-host "discord.com") 443))
         (payl (json/encode
                `((content . ,message)
                  (tts . #f)))))
    (tls/upgrade-connection con "discord.com")
    (print chan-id)
    (tls/write
     con
     (string-append
"POST /api/channels/" chan-id "/messages HTTP/1.1
Host: discord.com
Authorization: Bot " token "
Content-type: application/json
User-Agent: DiscordBot (https://krzysckh.org, 0)
Content-length: " (number->string (string-length payl)) "

" payl))
    (tls/close con)))
  ;; "embeds": [{
  ;;   "title": "Hello, Embed!",
  ;;   "description": "This is an embedded message."
  ;; }]
;; }


(define (ws-send c op payl)
  (let* ((payload
          (cond
           ((bytevector? payl) (bytevector->list payl))
           ((string? payl) (string->bytes payl))
           ((list? payl) payl)
           (else
            (error "unknown type for payload " payl))))
         (len (length payload))
         (len-bytes (if (>= len 126) ;; TODO:  assumes len < 0xffff
                        (list 126
                              (>> len 8)
                              (band len #xff))
                        (list len)))
         (data (append
                (list (bior #b10000000 op))
                (list (bior #b10000000 (car len-bytes)))
                (cdr len-bytes)
                mask
                (mask-payload payload mask))))

    (print "writing: " (list->string payload))
    (print "real-l:  " (length payload))
    (print "length:  " len-bytes)

    (tls/write c data)))

(define (identify c)
  (ws-send
   c 1
   (json/encode
    `((op . 2)
      (t . null)
      (s . null)
      (d
       . ((token . ,token)
          (properties
           . ((os . "Linux")
              (browser . "owl-lisp")
              (device  . "owl-lisp")))
          (compress . #f)
          (shard . (0 1))
          (presence
           . ((activities . ((name . "owl lisp") (type . 0)))
              (status . "sigma")
              (since . 91879201)
              (afk . #f)))
          (intents . 34563)
          ))))))

(define (asss str asc)
  (let loop ((asc asc))
    (cond
     ((not (string? (caar asc))) (loop (cdr asc)))
     ((string=? (caar asc) str) (car asc))
     (else
      (loop (cdr asc))))))

(define (start-heartbeats con j last-ack-s)
  (print j)
  (let* ((d (cdr (assq 'd j)))
         (interval (cdr (assq 'heartbeat_interval d))))
    (let loop ()
      (ws-send
       con 1
       (json/encode
        `((op . 1)
          (d . ,(if (null? (last-ack-s)) 'null (last-ack-s)))
          (t . null)
          (s . null))))
      (sleep interval)
      (loop))))

(define owl-mentined (string->regex "m/(owl)|(lisp)|(ovvl)|(sowa)/"))

(define (handle-bot con json bot-id)
  (print "handle-bot")
  (let ((d (cdr* (assq 'd json))))
    (cond
     ;; check if bot-id == sender id
     ((string=? (cdr* (assq 't json)) "GUILD_CREATE")
      (print "joined server: " (cdr* (assq 'name d))))
     ((string=? (cdr* (assq 't json)) "MESSAGE_CREATE")
      (let ((chan-id (cdr (assq 'channel_id d)))
            (uid (cdr (assq 'id (cdr (assq 'author d)))))
            (msg (cdr (assq 'content d))))
        (when (not (string=? uid bot-id))
          (when (owl-mentined msg)
            (send-message (cdr (assq 'channel_id d)) "owl mentioned - neuron activation")))))
      )))

(define hostname "gateway.discord.gg")

(lambda (_)
  (set-memory-limit (<< 2 20))
  (let ((c (open-connection (sys/resolve-host hostname) 443))
        (last-ack-s (make-variable 'last-ack-s)))
    (last-ack-s null)
    ;; TODO: last ack is unused, but according to discords' api
    ;; docs it should be lol lol

    (tls/upgrade-connection c hostname)
    (tls/write c
"GET /?v=6&format=json HTTP/1.1
Host: gateway.discord.gg
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Key: c2lnbWFwYXBpZXo=
Sec-WebSocket-Version: 13

")
    ;; the http response - i don't care about it
    (let loop ()
      (let ((l (tls/read c (<< 2 16))))
        (if (> (length l) 0)
            l
            (loop))))

    (print "ws ready")
    (let loop ((first? #t) (auth? #f) (bot-id 0))
      (let* ((b1 (car (force-read c 1)))
             (op (band b1 #b00001111))
             (fin? (band b1 #b10000000))
             (_ (when (not fin?)
                  (error-out "FUCK" "not fin?")))
             (len (car (force-read c 1)))
             (len (if (>= len 126)
                      (let ((b (force-read c 2)))
                        (bior (<< (car b) 8) (cadr b)))
                      len))
             (payl (list->string (force-read c len)))
             (json (parse-json payl)))

        (let* ((anything? auth?)
               (auth? (if (and (not first?) (not auth?) (= op 1))
                          (begin
                            (identify c)
                            #t)
                          auth?))
               (bot-id (if (string? (cdr* (assq 't json)))
                           (if (string=? (cdr (assq 't json)) "READY")
                               (cdr* (assq 'id (cdr* (assq 'user (cdr* (assq 'd json))))))
                               bot-id)
                           bot-id)))
          (cond
           (first?
            (thread 'heartbeat (start-heartbeats c json last-ack-s)))
           ((not anything?) #t)
           ((= op 8)
            (tls/close c)
            (error-out "discord requested connection close"))
           (else
            (handle-bot c json bot-id)))

          (print "ok loop")
          (loop #f auth? bot-id))))
    (tls/close c))
  0)
