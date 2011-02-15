#lang scheme

(provide (all-defined-out))


(define session-features 
  '((1 . long-password)
    (2 . found-rows)
    (4 . long-flag)
    (8 . connect-with-db)
    (16 . no-schema)
    (32 . compress)
    (64 . odbc)
    (128 . local-files)
    (256 . ignore-space)
    (512 . protocol-41)
    (1024 . interactive)
    (2048 . ssl)
    (4096 . ignore-sigpipe)
    (8192 . transactions)
    (16384 . reserved)
    (32768 . secure-connection)
    (65536 . multi-statements)
    (131072 . multi-results)))

(define (integer->features n)
  (let loop ((lst session-features) (res '()))
    (cond ((null? lst) res)
          (else
           (let ((f (car lst)))
             (cond ((zero? (bitwise-and n (car f))) (loop (cdr lst) res))
                   (else (loop (cdr lst) (cons (cdr f) res)))))))))

(define (features->integer fs)
  (let loop ((lst session-features) (res 0))
    (cond ((null? lst) res)
          (else
           (let ((f (car lst)))
             (cond ((memq (cdr f) fs) (loop (cdr lst) (bitwise-ior (car f) res)))
                   (else (loop (cdr lst) res))))))))


(define default-client-features
  '(long-password
    long-flag
    local-files
    protocol-41
    transactions
    secure-connection
    multi-statements
    multi-results))

  