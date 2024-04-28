;; -*- mode: scheme; compile-command: "ol -i lib/robusta -r ws.scm" -*-
(import
 (owl toplevel)
 (owl regex)
 (owl digest)
 (robusta encoding base64)
 (prefix (owl sys) sys/))

(define port 2137)

(define split-nl (string->regex "c/\r\n/"))
(define block-size (<< 2 16))

(define ws-magic "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
(define (drain-http-req fd acc)
  (let* ((b (try-get-block fd block-size #t))
         (l (split-nl (list->string (bytevector->list b)))))
    (if (has? l "")
        (append acc l)
        (drain-http-req fd (append acc l)))))

(define split-: (string->regex "c/: /"))
(define (http->alist l)
  (filter self (map (λ (s)
                      (let ((vs (split-: s)))
                        (if (= (length vs) 2)
                            (cons (string->symbol (car vs)) (cadr vs)) #f)))
                    l)))

(define (switch-to-ws fd key)
  (let ((s (string-append
"HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: " key "\n\n")))
    (write-bytes fd (string->bytes s))))

(define (unmask pl mask)
  (map
   (λ (n) (bxor (list-ref pl n) (list-ref mask (modulo n 4))))
   (iota 0 1 (length pl))))

(define (handle-block block)
  (let* ((b (car block))
         (fin (band b #b10000000))
         (op  (band b #b00001111)))
    (when (= fin 0)      (error "no support for continuations" fin))
    (when (not (= op 1)) (error "no support for opcode" op))
    (let* ((b (cadr block))
           (masked? (band b #b10000000))
           (size    (band b #b01111111)))
      (when (= masked? 0) (error "unmasked?" masked?))
      (when (> size 126)  (error "size > 126 unsupported"))
      (let* ((mask (take (cddr block) 4))
             (payload-masked (drop block 6)))
        (print payload-masked)
        (print (list->string (unmask payload-masked mask)))
        ))))


(define (read-stuff-forever fd)
  (let loop ((b (try-get-block fd block-size #t)))
    (handle-block (bytevector->list b))
    (loop (try-get-block fd block-size #t))))

(lambda (_)
  (print "READY")
  (let ((clients (tcp-clients port)))
    (let loop ((c (clients)))
      (let* ((fd (cdar c))
             (q (http->alist (drain-http-req fd '())))
             (key (cdr* (assq 'Sec-WebSocket-Key q))))
        (if (not key)
            (close-port fd) ;; not a websocket request
            (let ((response-key (list->base64 (sha1-bytes (string-append key ws-magic)))))
              (switch-to-ws fd response-key)
              (print "handshake ok")
              ;; (thread
               (read-stuff-forever fd)
               ;; )
               )))

      (loop ((cdr c)))))
  0)
