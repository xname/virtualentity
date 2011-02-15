#lang scheme

(require (file "charset.ss")
         (file "structs.ss")
         (file "session-features.ss")
         (file "packet-decoders.ss")
         (file "io.ss")
         (prefix-in srfi: srfi/19))

(provide check-protocol-version
         check-error-packet
         eof-packet?
         more-results?
         parse-server-version
         server-version>=?
         make-client-auth-packet
         decode-field-packet-sequence
         decode-text-row-packet-sequence
         decode-binary-row-packet-sequence
         encode-prepared-statement-arguments
         send-long-parameter
         get-parameter-type-bytes
         
         ;; packet types
         ERROR
         EOF
         OK
         
         ;; commands
         QUIT
         QUERY
         PREPARE
         STMT-EXEC
         STMT-SEND-LONG
         STMT-RESET
         STMT-CLOSE
         
         ;; server status flags
         IN-TRANSACTION
         AUTOCOMMIT
         MORE-RESULTS
         NO-GOOD-INDEX-USED
         NO-INDEX-USED
         CURSOR-EXISTS
         
         ;; FIELD TYPES
         T-DECIMAL
         T-TINY
         T-SHORT
         T-LONG
         T-FLOAT
         T-DOUBLE
         T-NULL
         T-TIMESTAMP
         T-LONGLONG
         T-INT24
         T-DATE
         T-TIME
         T-DATETIME
         T-YEAR
         T-NEWDATE
         T-VARCHAR
         T-BIT
         T-NEWDECIMAL
         T-ENUM
         T-SET
         T-TINY-BLOB
         T-MEDIUM-BLOB
         T-LONG-BLOB
         T-BLOB
         T-VAR-STRING
         T-STRING
         T-GEOMETRY
         )

;; field types
(define T-DECIMAL #x00)
(define T-TINY #x01)
(define T-SHORT #x02)
(define T-LONG #x03)
(define T-FLOAT #x04)
(define T-DOUBLE #x05)
(define T-NULL #x06)
(define T-TIMESTAMP #x07)
(define T-LONGLONG #x08)
(define T-INT24 #x09)
(define T-DATE #x0a)
(define T-TIME #x0b)
(define T-DATETIME #x0c)
(define T-YEAR #x0d)
(define T-NEWDATE #x0e)
(define T-VARCHAR #x0f)
(define T-BIT #x10)
(define T-NEWDECIMAL #xf6)
(define T-ENUM #xf7)
(define T-SET #xf8)
(define T-TINY-BLOB #xf9)
(define T-MEDIUM-BLOB #xfa)
(define T-LONG-BLOB #xfb)
(define T-BLOB #xfc)
(define T-VAR-STRING #xfd)
(define T-STRING #xfe)
(define T-GEOMETRY #xff)
  
;; packet types
(define ERROR #xff)
(define EOF #xfe)
(define OK #x00)

;; commands
(define QUIT #x01)
(define QUERY #x03)
(define PREPARE #x16)
(define STMT-EXEC #x17)
(define STMT-SEND-LONG #x18)
(define STMT-CLOSE #x19)
(define STMT-RESET #x1a)

;; server status flags
(define IN-TRANSACTION #x01)
(define AUTOCOMMIT #x02)
(define MORE-RESULTS #x08)
(define NO-GOOD-INDEX-USED #x10)
(define NO-INDEX-USED #x20)
(define CURSOR-EXISTS #x40)


;; Die if the server is too old.
(define (check-protocol-version version)
  (when (< version 10)
    (error (format "MySQL server uses an old protocol version (~a).  This driver supports version 10." version))))

;; Parse the server version string into a vector #(major minor subminor)
(define (parse-server-version str)
  (let ((res (regexp-match #rx"^.*?([0-9]+)\\.([0-9]+)\\.([0-9]+)" str)))
    (apply vector (map string->number (cdr res)))))

;; True if the server version is at least (major.minor.subminor)
(define (server-version>=? v major minor subminor)
  (let* ((s-major (vector-ref v 0))
         (s-minor (vector-ref v 1))
         (s-subminor (vector-ref v 2)))
    (or (> s-major major)
        (and (= s-major major) 
             (or (> s-minor minor) 
                 (and (= s-minor minor) (>= s-subminor subminor)))))))
  
;; is this an EOF packet?
(define (eof-packet? buf)
  (and (= (bytes-ref buf 0) EOF)
       (< (bytes-length buf) 9)))

;; #t iff there are more results on the way
(define (more-results? status)
  (not (zero? (bitwise-and MORE-RESULTS status))))
  
;; Check for an error packet
(define (check-error-packet p char-encoding)
  (let-values (((len seq buf) (read-packet p)))
    (when (= (bytes-ref buf 0) ERROR)
      (let-values (((fc errno marker sqlstate message) (decode-error buf 0 char-encoding)))
        (raise (make-exn:mysql errno sqlstate message))))
    (values len seq buf)))
        

;; create the client auth packet
(define (make-client-auth-packet features charset-byte user password-hash db)
  (let* ((features (if db (cons 'connect-with-db features) features))
         (features-int (features->integer features))
         (out (open-output-bytes)))
    (write-bytes (integer->integer-bytes features-int 4 #f #f) out)
    (write-bytes (integer->integer-bytes max-packet-size 4 #f #f) out)
    (write-byte charset-byte out)
    
    (when user
      ;; filler
      (write-bytes (make-bytes 23 0) out)
      ;; username
      (write-bytes (string->bytes/utf-8 user) out)
      (write-byte 0 out)
      ;;password
      (write-byte (bytes-length password-hash) out)
      (write-bytes password-hash out)
      ;; schema
      (when db
        (write-bytes (string->bytes/utf-8 db) out)
        (write-byte 0 out)))
    
    (get-output-bytes out)))


;; decodes a series of field packets
(define (decode-field-packet-sequence ip num-fields enc)
  (let ((res (make-vector num-fields)))
    (let loop ((i 0))
      (let-values (((len seq buf) (read-packet ip)))
        (if (eof-packet? buf)
            res
            (let-values (((catalog db table org-table name org-name charset disp-length type flags decimals default) (decode-field buf 0 enc)))
              (vector-set! res i 
                           (make-field catalog db table org-table name org-name (get-charset-name charset) disp-length type flags decimals default))
              (loop (add1 i))))))))




(define text-decoder-map
  (let ((v (make-vector 256))
        (str (λ (f x) 
               (if (eq? (field-character-encoding f) 'BINARY)
                   (string->bytes/latin-1 x)
                   x)))
        (num (λ (f x) (string->number x 10)))
        (datetime (λ (f x) (parse-datetime x)))
        (date (λ (f x) (parse-date x)))
        (time (λ (f x) (parse-time x))))
                
    
    (vector-set! v T-DECIMAL num)
    (vector-set! v T-TINY num)
    (vector-set! v T-SHORT num)
    (vector-set! v T-LONG num)
    (vector-set! v T-FLOAT num)
    (vector-set! v T-DOUBLE num)
    (vector-set! v T-NULL (λ (f x) sql-null))
    (vector-set! v T-TIMESTAMP datetime)
    (vector-set! v T-LONGLONG num)
    (vector-set! v T-INT24 num)
    (vector-set! v T-DATE date)
    (vector-set! v T-TIME time)
    (vector-set! v T-DATETIME datetime)
    (vector-set! v T-YEAR num)
    (vector-set! v T-NEWDATE date)
    (vector-set! v T-VARCHAR str)
    (vector-set! v T-BIT num)
    (vector-set! v T-NEWDECIMAL num)
    (vector-set! v T-ENUM str)
    (vector-set! v T-SET str)
    (vector-set! v T-TINY-BLOB str)
    (vector-set! v T-MEDIUM-BLOB str)
    (vector-set! v T-LONG-BLOB str)
    (vector-set! v T-BLOB str)
    (vector-set! v T-VAR-STRING str)
    (vector-set! v T-STRING str)
    (vector-set! v T-GEOMETRY str)
    
    v))


(define (decode-text-row-packet-sequence ip num-fields fields kons knil)
  (let loop ((rows knil))
    (let-values (((len seq buf) (read-packet ip)))
      (if (eof-packet? buf)
          (let-values (((fc wc status) (decode-eof buf 0)))
            (values rows (more-results? status)))
          (let inner ((i 0) (pos 0) (row '()))
            (if (= i num-fields)
                (loop (kons row rows))
                (let*-values (((f) (vector-ref fields i))
                              ((enc fn) (values (field-character-encoding f) (vector-ref text-decoder-map (field-type f))))
                              ((raw-val pos) (decode-lcs buf pos enc))
                              ((val) (if (sql-null? raw-val)
                                         raw-val
                                         (fn f raw-val))))
                  (inner (add1 i) pos (cons val row)))))))))
        

(define binary-decoder-map
    (let ((v (make-vector 256))
          (int1 (λ (f buf pos enc)
                  (let* ((signed? (zero? (bitwise-and 32 (field-flags f))))
                         (v (bytes-ref buf pos))
                         (v (if (and signed? (>= v 128)) (- v 256) v)))
                    (values v (add1 pos)))))
          (int2 (λ (f buf pos enc) 
                  (let ((signed? (zero? (bitwise-and 32 (field-flags f))))
                        (end (+ pos 2)))
                    (values (integer-bytes->integer buf signed? #f pos end) end))))
          (int4 (λ (f buf pos enc) 
                  (let* ((signed? (zero? (bitwise-and 32 (field-flags f))))
                         (end (+ pos 4)))
                    (values (integer-bytes->integer buf signed? #f pos end) end))))
          (int8 (λ (f buf pos enc) 
                  (let* ((signed? (zero? (bitwise-and 32 (field-flags f))))
                         (end (+ pos 8)))
                    (values (integer-bytes->integer buf signed? #f pos end) end))))
          
          (datetime (λ (f buf pos enc)
                      (let-values (((len pos) (decode-lcb buf pos)))
                        (cond ((zero? len) (values zero-date pos))
                              (else (let* ((year (integer-bytes->integer buf #f #f pos (+ pos 2)))
                                           (month (bytes-ref buf (+ pos 2)))
                                           (day (bytes-ref buf (+ pos 3)))
                                           (res
                                            (cond ((> len 4)
                                                   (srfi:make-date 0
                                                                   (bytes-ref buf (+ pos 6))
                                                                   (bytes-ref buf (+ pos 5))
                                                                   (bytes-ref buf (+ pos 4))
                                                                   day month year (current-client-timezone-offset)))
                                                  (else
                                                   (srfi:make-date 0 0 0 0 day month year (current-client-timezone-offset))))))
                                      (values res (+ pos len))))))))
          
          (str (λ (f buf pos enc)
                 (let*-values (((len pos) (decode-lcb buf pos))
                               ((end) (+ pos len))
			       ((str) (if (eq? enc 'BINARY)
					  (subbytes buf pos end)
					  (bytes->string/enc buf pos end enc))))
                   (values str end))))
          
          (dec (λ (f buf pos enc)
                 (let*-values (((len pos) (decode-lcb buf pos))
                               ((end) (+ pos len)))
                   (values (string->number (bytes->string/enc buf pos end enc)) end)))))
                   
                 
      
      (vector-set! v T-NULL (λ (f buf pos enc) (values sql-null pos)))
      
      (vector-set! v T-TINY int1)
      (vector-set! v T-BIT int1)
      (vector-set! v T-SHORT int2)
      (vector-set! v T-YEAR int2)
      (vector-set! v T-LONG int4)
      (vector-set! v T-INT24 int4)
      (vector-set! v T-LONGLONG int8)
      
      (vector-set! v T-FLOAT (λ (f buf pos enc)
                               (let ((end (+ pos 4)))
                                 (values (floating-point-bytes->real buf #f pos end) end))))
      (vector-set! v T-DOUBLE (λ (f buf pos enc)
                                (let ((end (+ pos 8)))
                                  (values (floating-point-bytes->real buf #f pos end) end))))
      
      (vector-set! v T-TIME (λ (f buf pos enc)
                              (let-values (((len pos) (decode-lcb buf pos)))
                                (cond ((zero? len) (values zero-time pos))
                                      (else (let*-values (((day hour min sec)
                                                           (values (integer-bytes->integer buf #f #f (add1 pos) (+ pos 5))
                                                                   (bytes-ref buf (+ pos 5))
                                                                   (bytes-ref buf (+ pos 6))
                                                                   (bytes-ref buf (+ pos 7))))
                                                          ((res) (+ (* 60 60 24 day)
                                                                    (* 60 60 hour)
                                                                    (* 60 min)
                                                                    sec))
                                                          ((res) (if (zero? (bytes-ref buf pos)) res (- res))))
                                              (values (srfi:make-time 'time-duration 0 res) (+ pos len))))))))
      
      (vector-set! v T-DATE (λ (f buf pos enc)
                              (let-values (((len pos) (decode-lcb buf pos)))
                                (cond ((zero? len) (values zero-date pos))
                                      (else (values (srfi:make-date 0 0 0 0
                                                                    (bytes-ref buf (+ pos 3))
                                                                    (bytes-ref buf (+ pos 2))
                                                                    (integer-bytes->integer buf #f #f pos (+ pos 2))
                                                                    (current-client-timezone-offset))
                                                    (+ pos len)))))))
      
      (vector-set! v T-DATETIME datetime)
      (vector-set! v T-TIMESTAMP datetime)
      
      (vector-set! v T-VAR-STRING str)
      (vector-set! v T-STRING str)
      (vector-set! v T-VARCHAR str)
      
      (vector-set! v T-BLOB str)
      (vector-set! v T-TINY-BLOB str)
      (vector-set! v T-MEDIUM-BLOB str)
      (vector-set! v T-LONG-BLOB str)
      
      (vector-set! v T-DECIMAL dec)
      (vector-set! v T-NEWDECIMAL dec)
      
      (vector-set! v T-GEOMETRY str)
      v))

(define (decode-binary-row-packet-sequence ip num-fields fields kons knil)
  (define (next-byte/bit byte bit)
    (if (= bit 128)
        (values (add1 byte) 1)
        (values byte (arithmetic-shift bit 1))))
  
  (let ((null-count (quotient (+ num-fields 9) 8)))
    
    (let loop ((rows knil))
      (let-values (((len seq buf) (read-packet ip)))
        (if (eof-packet? buf)
            rows
            (let inner ((i 0) (pos (add1 null-count)) (row '()) (byte 1) (bit 4))
              (if (= i num-fields)
                  (loop (kons row rows))
                  (cond ((not (zero? (bitwise-and (bytes-ref buf byte) bit)))
                         (let-values (((byte bit) (next-byte/bit byte bit)))
                           (inner (add1 i) pos (cons sql-null row) byte bit)))
                        (else
                         (let*-values (((f) (vector-ref fields i))
                                       ((enc fn) (values (field-character-encoding f) (vector-ref binary-decoder-map (field-type f))))
                                       ((val pos) (fn f buf pos enc))
                                       ((byte bit) (next-byte/bit byte bit)))
                           (inner (add1 i) pos (cons val row) byte bit)))))))))))

(define (encode-prepared-statement-value encoded-values val encoding param)
  
  (define (write-lcb buf)
    (let ((len (bytes-length buf)))
      (cond ((<= len 250)
             (write-byte len encoded-values))
            ((<= len 65535) 
             (write-byte 252 encoded-values)
             (write-bytes (integer->integer-bytes len 2 #f #f) encoded-values))
            ((<= len 1677215)
             (write-byte 253 encoded-values)
             (write-bytes (integer->integer-bytes len 4 #f #f) encoded-values 0 3))
            (else
             (write-byte 254 encoded-values)
             (write-bytes (integer->integer-bytes len 8 #f #f) encoded-values)))
      (write-bytes buf encoded-values)))
  
  (define (encode-string str)
    (write-lcb (string->bytes/enc str encoding))
    T-VAR-STRING)

  (define (encode-datetime val)
    ;; length is 7 bytes (w/o microseconds, which can't be stored anyway)
    (write-byte 7 encoded-values)
    (write-bytes (integer->integer-bytes (srfi:date-year val) 2 #f #f) encoded-values)
    (write-byte (srfi:date-month val) encoded-values)
    (write-byte (srfi:date-day val) encoded-values)
    (write-byte (srfi:date-hour val) encoded-values)
    (write-byte (srfi:date-minute val) encoded-values)
    (write-byte (srfi:date-second val) encoded-values)
    T-DATETIME)
  
  (define (encode-time val)
    (let*-values (((val) (srfi:add-duration val zero-time)) ;; lame...
                  ((ns sec) (values (srfi:time-nanosecond val)
                                    (srfi:time-second val)))
                  ((sec) (if (>= ns 500000000) (add1 sec) sec))
                  ((neg sec) (if (negative? sec) (values 1 (abs sec)) (values 0 sec)))
                  
                  ((day sec) (quotient/remainder sec (* 60 60 24)))
                  ((hour sec) (quotient/remainder sec (* 60 60)))
                  ((min sec) (quotient/remainder sec 60)))
           
      ;; length is 8 bytes
      (write-byte 8 encoded-values)
      ;; 1 if negative, 0 otherwise
      (write-byte neg encoded-values)
      ;; day, hour, minute, second
      (write-bytes (integer->integer-bytes day 4 #f #f) encoded-values)
      (write-byte hour encoded-values)    
      (write-byte min encoded-values)
      (write-byte sec encoded-values)
      T-TIME))

  (cond ((integer? val)
         ;; encode integers to use minimal space
         (let*-values (((q r) (quotient/remainder (integer-length val) 8))
                       ((bytelen) (if (zero? r) q (add1 q))))
           (cond ((> bytelen 8) 
                  (encode-string (number->string val)))
                 ((> bytelen 4) 
                  (write-bytes (integer->integer-bytes val 8 #t #f) encoded-values)
                  T-LONGLONG)
                 ((> bytelen 2)
                  (write-bytes (integer->integer-bytes val 4 #t #f) encoded-values)
                  T-LONG)
                 ((> bytelen 1)
                  (write-bytes (integer->integer-bytes val 2 #t #f) encoded-values)
                  T-SHORT)
                 (else
                  (write-byte (bitwise-and val #xff) encoded-values)
                  T-TINY))))
        ;; encode non-integral numbers as strings
        ((number? val)
         (let ((dec (field-decimals param)))
           (cond ((and (positive? dec) (real? val))
                  (encode-string (real->decimal-string val (add1 dec))))
                 (else
                  (encode-string (number->string (exact->inexact val)))))))
        ;; encode strings as VAR-STRING
        ((string? val)
         (encode-string val))
        ;; dates and datetimes
        ;; NOTE: still don't know the right way to handle timezone issues
        ((srfi:date? val)
         (encode-datetime val))
        ;; times
        ((srfi:time? val)
         (encode-time val))
        
        ;; now, some oddball data...
        
        ;; encode symbols as strings
        ((symbol? val)
         (encode-string (symbol->string val)))
        ;; encode bytes as VAR-STRING
        ((bytes? val)
         (write-lcb val)
         T-VAR-STRING)
        ;; encode booleans as tiny ints
        ((boolean? val)
         (write-byte (if val 1 0) encoded-values)
         T-TINY)
       
        (else
         (raise-type-error 'execute "a scheme value that can be marshaled as a prepared statement parameter argument" val))))
                  

(define (encode-prepared-statement-arguments stmt args)
  (let* ((num-params (prepared-statement-parameter-count stmt))
         (null-bitmap (make-bytes (quotient (+ num-params 7) 8)))
         (encoded-values (open-output-bytes))
         (enc (connection-character-encoding (prepared-statement-connection stmt)))
         (params (prepared-statement-parameters stmt))
         (types (prepared-statement-types stmt))
         (initial-exec? (or (zero? num-params) (not (vector-ref types 0)))))
    
    (let loop ((i 0) (xs args) (long-params '()) (types-changed? initial-exec?) (needs-reset? #f))
      (let-syntax ((next (syntax-rules ()
                           ((_ new-type lp)
                            (let* ((old-type (vector-ref types i))
                                   (same-type? (eqv? old-type new-type))
                                   (changed? (or types-changed? (not same-type?)))
                                   (lps (if lp (cons lp long-params) long-params))
                                   (reset? (or needs-reset? lp (and (eqv? old-type T-BLOB)
                                                                    (not (eqv? new-type T-BLOB))))))
                              (when (not same-type?)
                                (vector-set! types i new-type))
                              (loop (add1 i) (cdr xs) lps changed? reset?))))))
        
        (cond ((null? xs)
               (values (and (not initial-exec?) needs-reset?) ;; need to reset the statement
                       types-changed? ;; need to resend the types
                       (get-output-bytes encoded-values) ;; the encoded parameter values
                       null-bitmap ;; nulls
                       long-params)) ;; the long parameters
                       
              (else
               (let ((x (car xs)))
                 (cond ((sql-null? x)
                        (let*-values (((byte-index bit-index) (quotient/remainder i 8))
                                      ((byte-val) (bytes-ref null-bitmap byte-index)))
                          (bytes-set! null-bitmap byte-index (bitwise-ior byte-val (arithmetic-shift 1 bit-index)))
                          (next T-NULL #f)))
                     
                       ((input-port? x)
                        (next T-BLOB (cons i x)))
                       
                       (else 
                        (let ((t (encode-prepared-statement-value encoded-values x enc (vector-ref params i))))
                          (next t #f)))))))))))


(define (send-long-parameter stmt i in)
  (let ((buf (make-bytes 8192))
        (out (connection-output-port (prepared-statement-connection stmt)))
        (header (bytes-append (bytes STMT-SEND-LONG)
                              (integer->integer-bytes (prepared-statement-id stmt) 4 #f #f)
                              (integer->integer-bytes i 2 #f #f))))
    (let-syntax ((send-chunk (syntax-rules ()
                               ((_ packet)
                                (let ((seq (prepared-statement-sequence-count stmt)))
                                  ;(set-prepared-statement-sequence-count! stmt (add1 seq))
                                  (send-packet seq packet out))))))
      
      (let loop ((p header) (num-read 0) (total-sent 0) (total-read 0))
        (let ((n (read-bytes! buf in)))
          (cond ((eof-object? n)
                 (when (> total-read total-sent)
                   (send-chunk p)))
                ((>= (+ num-read n) long-packet-size)
                 (send-chunk (bytes-append p (subbytes buf 0 n)))
                 (loop header 0 (+ total-sent num-read n) (+ total-read n)))
                (else
                 (loop (bytes-append p (subbytes buf 0 n)) (+ num-read n) total-sent (+ total-read n)))))))))


(define (get-parameter-type-bytes stmt send?)
  (cond ((not send?)
         (bytes 0))
        (else
         (let ((out (open-output-bytes))
               (n (prepared-statement-parameter-count stmt))
               (types (prepared-statement-types stmt)))
           (write-byte 1 out)
           (let loop ((i 0))
             (cond ((< i n)
                    (let ((t (vector-ref types i)))
                      (write-bytes (integer->integer-bytes t 2 #f #f) out)
                      (loop (add1 i))))
                   (else
                    (get-output-bytes out))))))))
  