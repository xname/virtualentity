#lang scheme

(require (file "charset.ss")
         (file "structs.ss"))

(provide
 decode-lcs
 decode-lcb
 decode-handshake
 decode-ok
 decode-eof
 decode-error
 decode-result-set-header
 decode-field
 decode-prep-init
 )

;; Decoding MySQL packets
;;

;; utilities
(define-syntax decode-byte
  (syntax-rules ()
    ((_ buf pos) (values (bytes-ref buf pos) (add1 pos)))))
  
(define-syntax decode-int
  (syntax-rules ()
    ((_ buf pos len) 
     (let ((end (+ pos len)))
       (values (integer-bytes->integer buf #f #f pos end) end)))))
               
(define-syntax decode-bytes
  (syntax-rules ()
    ((_ buf pos len)
     (let ((end (+ pos len)))
       (values (subbytes buf pos end) end)))))

;; nts = "null terminated string"
(define (decode-nts buf pos char-encoding)
  (let loop ((i pos))
    (cond ((zero? (bytes-ref buf i)) (values (bytes->string/enc buf pos i char-encoding) (add1 i)))
          (else (loop (add1 i))))))

;; lcb = "length coded binary"
(define (decode-lcb buf pos)
  (let-values (((b pos) (decode-byte buf pos)))
    (case b
      ((251) (values sql-null pos))
      ((252) (decode-int buf pos 2))
      ((253) (let*-values (((low-val pos) (decode-int buf pos 2))
                           ((high-val pos) (values (bytes-ref buf pos) (add1 pos)))
                           ((val) (bitwise-ior low-val (arithmetic-shift high-val 16))))
               (values val pos)))
      ((254) (decode-int buf pos 8))
      (else (values b pos)))))

;; lcs = "length coded string"
(define (decode-lcs buf pos char-encoding)
  (let-values (((len pos) (decode-lcb buf pos)))
    (cond ((sql-null? len) (values len pos))
          (else (let ((end (+ pos len)))
                  (values (bytes->string/enc buf pos end char-encoding) end))))))

;; "end string" is a string that goes to the end of the packet
(define (decode-end-string buf pos char-encoding)
  (let ((end (bytes-length buf)))
    (if (> end pos)
        (values (bytes->string/enc buf pos end char-encoding) end)
        (values "" end))))


;; packet decoding
(define (decode-handshake buf pos)
  (let*-values (((protocol-version pos) (decode-byte buf pos))
                ((server-version pos) (decode-nts buf pos 'ISO-8859-1))
                ((thread-id pos) (decode-int buf pos 4))
                ((salt1 pos) (decode-bytes buf pos 8))
                ((pos) (add1 pos))
                ((server-features pos) (decode-int buf pos 2))
                ((server-encoding pos) (decode-byte buf pos))
                ((server-status pos) (decode-int buf pos 2))
                ((pos) (+ pos 13)) ;; unused space in packet
                ((salt2 pos) (decode-bytes buf pos 12)))
    (values protocol-version
            server-version
            thread-id
            salt1
            salt2
            server-features
            server-encoding
            server-status)))

(define (decode-ok buf pos char-encoding)
  (let*-values (((field-count pos) (decode-lcb buf pos))
                ((affected-rows pos) (decode-lcb buf pos))
                ((insert-id pos) (decode-lcb buf pos))
                ((server-status pos) (decode-lcb buf pos))
                ((warning-count pos) (decode-lcb buf pos))
                ((message pos) (if (> (bytes-length buf) pos)
                                   (decode-end-string buf pos char-encoding)
                                   (values #f pos))))
    (values field-count
            affected-rows
            insert-id
            server-status
            warning-count
            message)))

(define (decode-error buf pos char-encoding)
  (let*-values (((field-count pos) (decode-byte buf pos))
                ((errno pos) (decode-int buf pos 2))
                ((marker pos) (decode-byte buf pos))
                ((sqlstate pos) (decode-bytes buf pos 5))
                ((message pos) (decode-end-string buf pos char-encoding)))
    (values field-count
            errno
            marker
            sqlstate
            message)))

(define (decode-result-set-header buf pos)
  (let*-values (((field-count pos) (decode-lcb buf pos))
                ((extra pos) (if (> (bytes-length buf) pos)
                                 (decode-lcb buf pos)
                                 (values #f pos))))
    (values field-count
            extra)))

(define (decode-field buf pos char-encoding)
  (let*-values (((catalog pos) (decode-lcs buf pos char-encoding))
                ((db pos) (decode-lcs buf pos char-encoding))
                ((table pos) (decode-lcs buf pos char-encoding))
                ((org-table pos) (decode-lcs buf pos char-encoding))
                ((name pos) (decode-lcs buf pos char-encoding))
                ((org-name pos) (decode-lcs buf pos char-encoding))
                ((pos) (add1 pos))
                ((charset pos) (decode-int buf pos 2))
                ((disp-length pos) (decode-int buf pos 4))
                ((type pos) (decode-byte buf pos))
                ((flags pos) (decode-int buf pos 2))
                ((decimals pos) (decode-byte buf pos))
                ((pos) (+ pos 2))
                ((default pos) (if (> (bytes-length buf) pos)
                                   (decode-lcb buf pos)
                                   (values #f pos))))
    (values catalog
            db
            table
            org-table
            name
            org-name
            charset
            disp-length
            type
            flags
            decimals
            default)))



(define (decode-eof buf pos)
  (let*-values (((field-count pos) (decode-byte buf pos))
                ((warning-count pos) (decode-int buf pos 2))
                ((status pos) (decode-int buf pos 2)))
    (values field-count
            warning-count
            status)))

(define (decode-row-data buf pos char-encoding)
  (let-values (((res pos) (decode-lcs buf pos char-encoding)))
    res))

(define (decode-prep-init buf pos)
  (let*-values (((field-count pos) (decode-lcb buf pos))
                ((statement-id pos) (decode-int buf pos 4))
                ((num-columns pos) (decode-int buf pos 2))
                ((num-parameters pos) (decode-int buf pos 2)))
    (values field-count
            statement-id
            num-columns
            num-parameters)))