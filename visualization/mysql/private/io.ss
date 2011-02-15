#lang scheme

(provide max-packet-size
         long-packet-size
         read-packet
         send-packet
         )

(define max-packet-size (- (expt 256 3) 1))
(define long-packet-size (* 1024 1024))


;; Reads a packet header, returns 2 values: 
;; - the length of the packet 
;; - and its sequence number
(define (read-packet-header p)
  (let ((data (read-bytes 4 p)))
    (values (bitwise-and (integer-bytes->integer data #f #f) #x00FFFFFF)
            (bytes-ref data 3))))

;; Read a packet from a port, returns 3 values:
;; - the length of the packet
;; - the packet's sequence number
;; - the body of the packet as a byte vector
;;
;; This procedure automatically coalesces split packets
(define (read-packet p)
  (let-values (((len seq) (read-packet-header p)))
    (let loop ((len len) (seq seq) (buf (read-bytes len p)))
      (if (< len max-packet-size)
          (values (bytes-length buf) seq buf)
          (let-values (((new-len new-seq) (read-packet-header p)))
            (loop new-len new-seq (bytes-append buf (read-bytes new-len p))))))))


;; private: used by send-packet to send a single network packet
(define (real-send-packet seq body start len p)
  (let ((len-bytes (integer->integer-bytes len 4 #f #f))
        (end (+ start len)))
    (write-bytes (subbytes len-bytes 0 3) p)
    (write-byte seq p)
    (write-bytes body p start end)))

;; Sends a packet to a port
;; Returns the sequence number of the last packet that was sent
;; Automatically splits large packets
(define (send-packet seq body p) 
  (let loop ((len (bytes-length body)) (seq seq) (i 0))
    (if (< len max-packet-size) 
        (begin (real-send-packet seq body i len p)
               (flush-output p)
               seq)
        (begin (real-send-packet seq body i max-packet-size p)
               (loop (- len max-packet-size) (+ seq 1) (+ i max-packet-size))))))

