;; -*- compile-command: "ol -i lib/robusta/ -x c ws-client.scm | clang -ggdb -DPRIM_CUSTOM -I/home/kpm/nmojeprogramy/owl/c tls.c -x c - -Wall -Wextra -ltls -o ws-client -fsanitize=address && ./ws-client" -*-

(import
 (owl toplevel)
 (owl variable)
 (prefix (owl sys) sys/)
 (robusta encoding base64)
 (prefix (robusta encoding json) json/)
 (prefix (tls) tls/))

(define token (read (open-input-file "token")))

(define (force-read c n)
  (let loop ((n n) (acc '()))
    (if (= n 0)
        acc
        (let* ((l (tls/read c n)))
          (sleep 100)
          (loop (- n (length l))
                (append acc l))))))

(define mask '(21 37 42 11)) ;; YEAHH
(define (mask-payload payload mask)
  (map
   (λ (n) (bxor (list-ref payload n)
                (list-ref mask (modulo n 4))))
   (iota 0 1 (length payload))))

(define (ws-send c op payl)
  (let* ((payload
          (cond
           ((bytevector? payl) (bytevector->list payl))
           ((string? payl) (string->bytes payl))
           ((list? payl) payl)
           (else
            (error "unknown type for payload " payl))))
         (len (length payload))
         (len-bytes (if (>= len 127)             ;; TODO:  assumes len < 0xffff
                        (list 127
                              (band len #xff00)
                              (band len #xff))
                        (list len)))
         (data (append
                (list (bior #b10000000 op))
                (list (bior #b10000000 (car len-bytes)))
                (cdr len-bytes)
                mask
                (mask-payload payload mask))))

    (print "writing: " (list->string payload))
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
          (intents . ,(<< 1 15))))))))

(define (asss str asc)
  (let loop ((asc asc))
    (cond
     ((not (string? (caar asc))) (loop (cdr asc)))
     ((string=? (caar asc) str) (car asc))
     (else
      (loop (cdr asc))))))

(define (start-heartbeats con payl last-ack-s)
  (let* ((j (json/decode payl))
         (d (cdr (asss "d" j)))
         (interval (cdr (asss "heartbeat_interval" d))))
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

(define hostname "gateway.discord.gg")

(lambda (_)
  (let ((c (open-connection (sys/resolve-host hostname) 443))
        (last-ack-s (make-variable 'last-ack-s)))
    (last-ack-s null)
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
    (let loop ((first? #t))
      (let* ((b1 (car (force-read c 1)))
             (op (band b1 #b00001111))
             (len (car (force-read c 1)))
             (payl (force-read c len)))
        (print "opcode: " op)
        (print "len: " len)
        (print "payload: " (list->string payl))

        (when first?
          (thread
           'heartbeat
           (start-heartbeats c (list->string payl) last-ack-s)))

        (when (and (not first?) (= op 1))
          (identify c))

        (when (= op 8)
          (print "discord requested connection close")
          (catch-thread 'heartbeat)
          (catch-thread 'auth)
          (tls/close c)
          (exit-owl 1)))

      (print "ok loop")
      (loop #f))

    (tls/close c))
  0)
