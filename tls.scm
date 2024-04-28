(define-library
    (tls)

  (import
   (owl toplevel)
   (owl lazy)
   (owl sys))

  (export
   init
   upgrade-connection
   write
   read
   close)

  (begin
    (define (init)
      (sys-prim 1000 #f #f #f))

    (define (upgrade-connection fd server-name)
      (sys-prim 1001 fd (c-string server-name) #f))

    (define (write ptr thing)
      (cond
       ((bytevector? thing) (sys-prim 1002 ptr (bytevector->list thing) #f))
       ((string? thing)     (sys-prim 1002 ptr (string->bytes thing) #f))
       ((list? thing)       (sys-prim 1002 ptr thing #f))
       (else
        (error "tls-write: unknown type " thing))))

    (define (read ptr n)
      (sys-prim 1003 ptr n #f))

    (define (close ptr)
      (sys-prim 1004 ptr #f #f))
    ))
