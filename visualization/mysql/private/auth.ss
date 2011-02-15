#lang scheme

;(require (planet soegaard/digest:1:2/digest))
(require (file "digest/digest.ss"))

(provide scramble-411
         make-retro-password-bytes)


;; The 4.1 protocol hash algorithm
(define (scramble-411 password-bytes seed-bytes)
  (let* ((stage1 (bytes-digest password-bytes 'sha1))
         (stage2 (bytes-digest stage1 'sha1))
         (raw (bytes-digest (bytes-append seed-bytes stage2) 'sha1)))
    (list->bytes (map bitwise-xor (bytes->list raw) (bytes->list stage1)))))


  
;; The old hash algorithm, used for old passwords...
(define (make-retro-password-bytes buf salt)
    
  (define ring-size (expt 2 64))
  (define (mod+ x y) (modulo (+ x y) ring-size))
  (define (mod* x y) (modulo (* x y) ring-size))
  (define (mod-shift x y) (modulo (arithmetic-shift x y) ring-size))
    
  (define (hash buf) 
    (let ((len (bytes-length buf)))
      (let loop ((n1 1345345333) (n2 305419889) (add 7) (i 0))
        (if (= i len)
            (values (bitwise-and #x7fffffff n1) (bitwise-and #x7fffffff n2))
            (let ((c (bytes-ref buf i)))
              (if (or (= c 32) (= c 9))
                  (loop n1 n2 add (+ i 1))
                  (let* ((nr-shift (mod-shift n1 8))
                         (nr-mask (bitwise-and 63 n1))
                         (nr (mod+ nr-shift (mod* c (mod+ add nr-mask))))
                         (nr2-shift (mod-shift n2 8))
                         (new-n1 (bitwise-xor nr n1)))
                    (loop new-n1 (mod+ n2 (bitwise-xor nr2-shift new-n1)) (mod+ add c) (+ i 1)))))))))
  
  (define (rnd val offset)
    (let ((d (/ val (- (expt 2 30) 1))))
      (bitwise-and #xff (inexact->exact (floor (+ (* d 31) offset))))))
  
  (let ((len (bytes-length salt)))
    (if (zero? (bytes-length buf))
        buf
        (let-values (((pw1 pw2) (hash salt))
                     ((msg1 msg2) (hash buf))
                     ((new-buf) (make-bytes (+ len 1) 0))
                     ((m) (- (expt 2 30) 1)))
          (let loop ((seed1 (remainder (bitwise-xor pw1 msg1) m)) (seed2 (remainder (bitwise-xor pw2 msg2) m)) (i 0))
            (if (= i len)
                (let* ((seed1 (remainder (mod+ seed2 (mod* seed1 3)) m))
                       (seed2 (remainder (mod+ seed1 (mod+ seed2 33)) m))
                       (b (rnd seed1 0)))
                  (let loop2 ((j 0))
                    (if (= j len)
                        new-buf
                        (begin (bytes-set! new-buf j (bitwise-xor (bytes-ref new-buf j) b))
                               (loop2 (+ j 1))))))
                (let* ((seed1 (remainder (mod+ seed2 (mod* seed1 3)) m))
                       (seed2 (remainder (mod+ seed1 (mod+ seed2 33)) m))
                       (b (rnd seed1 64)))
                  (bytes-set! new-buf i b)
                  (loop seed1 seed2 (+ i 1)))))))))
