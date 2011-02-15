#lang scheme

(require (prefix-in srfi-19: srfi/19)
         (only-in srfi/13 string-index)
         (only-in srfi/43 vector-map)
         (file "private/structs.ss"))

(provide format-sql)

(define hex-chars
  (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))

;; Used to format SQL strings.  Like PLT's format, but with special, MySQL-specific formatting / escaping
;;  ~s formats the next argument as a SQL string; the argument is quoted and escaped; arg must be a string
;;  ~d formats the next argument as a SQL DATE ('yyyy-mm-dd'); the argument must be an SRFI 19 date
;;  ~D formats the next argument as a SQL DATETIME ('yyyy-mm-dd hh:mm:ss'); the argument must be an SRFI 19 date
;;  ~t formats the next argument as a SQL TIME; the argument must be an SRFI 19 time
;;  ~i formats the next argument as an integer; arg must be an integer
;;  ~r formats the next argument as a real number; arg must be a real number
;;  ~c formats the next argument as a table or column name, escaping it properly; arg must be a string or symbol
;;  ~b formats the next argument as a byte string
;;  ~~ results in a tilde (~)
(define (format-sql str . values)
    
  (define (output-string out obj)
    (when (not (string? obj))
      (raise-type-error 'format-sql "Expected a string for ~s pattern" obj))
    (let ((len (string-length obj)))
      (write-char #\' out)
      (let loop ((i 0))
        (when (< i len)
          (let ((c (string-ref obj i)))
            (let-syntax ((escape 
                          (syntax-rules ()
                            ((_ c (x y) ...)
                             (case c
                               ((x) (write-char #\\ out)
                                    (write-char y out))
                               ...
                               (else (write-char c out)))))))
              (escape c
                      (#\nul #\0)
                      (#\u000a #\n)
                      (#\u000d #\r)
                      (#\\ #\\)
                      (#\' #\')
                      (#\" #\")
                      (#\u001a #\Z))
              (loop (add1 i))))))
      (write-char #\' out)))
    
  (define (output-date out obj)
    (when (not (srfi-19:date? obj))
      (raise-type-error 'format-sql "Expected SRFI 19 date for ~d pattern" obj))
    (fprintf out "'~a'" (srfi-19:date->string obj "~1")))
    
  (define (output-datetime out obj)
    (when (not (srfi-19:date? obj))
      (raise-type-error 'format-sql "Expected SRFI 19 date for ~D pattern" obj))
    (fprintf out "'~a'" (srfi-19:date->string obj "~5")))
    
  (define (output-time out obj)
    (when (not (srfi-19:time? obj))
      (raise-type-error 'format-sql "Expected SRFI 19 time for ~t pattern" obj))
    (let*-values (((seconds) (srfi-19:time-second obj))
                  ((minutes seconds) (quotient/remainder seconds 60))
                  ((hours minutes) (quotient/remainder minutes 60)))
      (fprintf out "'~a:~a:~a'" hours minutes seconds)))
  
  (define (output-integer out obj)
    (when (not (integer? obj))
      (raise-type-error 'format-sql "Expected integer for ~i pattern" obj))
    (display obj out))
    
  (define (output-real out obj)
    (when (not (real? obj))
      (raise-type-error 'format-sql "Expected real number for ~r pattern" obj))
    (display (number->string (exact->inexact obj)) out))
    
  (define (output-column-name out obj)
    (when (not (or (symbol? obj) (string? obj)))
      (raise-type-error 'format-sql "Expected symbol or string for ~c pattern" obj))
    (fprintf out "`~a`" obj))
    
  (define (output-bytes out obj)
    (when (not (bytes? obj))
      (raise-type-error 'formal-sql "Expected byte string for ~a pattern with blob type" obj))
    (let ((len (bytes-length obj)))
      (write-char #\x out)
      (write-char #\' out)
      (let loop ((i 0))
        (when (< i len)
          (let*-values (((b) (bytes-ref obj i))
                        ((low high) (quotient/remainder b 16)))
            (write-char (vector-ref hex-chars low) out)
            (write-char (vector-ref hex-chars high) out)
            (loop (add1 i)))))
      (write-char #\' out)))
            
       
  (define (output-typed out obj)
    (let ((type (car obj))
          (val (cdr obj)))
      (case type
        ((int) (output-integer out val))
        ((char varchar text enum) (output-string out val))
        ((decimal) (output-real out val))
        ((datetime) (output-datetime out val))
        ((date) (output-date out val))
        ((time) (output-time out val))
        ((blob) (output-bytes out val))
        (else (raise-mismatch-error 'format-sql "unknown escape character" type)))))
  
  
  (let ((len (string-length str))
        (out (open-output-string)))
    (let loop ((start 0) (argv values))
      (let ((i (string-index str #\~ start)))
        (if i
            (if (or (null? argv) (= i len))
                (raise-mismatch-error 'format-sql "Number of format patterns does not match the number of objects:" (length values))
                (let ((c (string-ref str (+ i 1)))
                      (next-start (+ i 2))
                      (obj (car argv)))
                  (display (substring str start i) out)
                  
                  (if (sql-null? obj)
                      (display "NULL" out)
                      (case c
                        ((#\a) (display obj out))
                        ((#\s) (output-string out obj))
                        ((#\d) (output-date out obj))
                        ((#\D) (output-datetime out obj))
                        ((#\t) (output-time out obj))
                        ((#\i) (output-integer out obj))
                        ((#\r) (output-real out obj))
                        ((#\c) (output-column-name out obj))
                        ((#\b) (output-bytes out obj))
                        ((#\*) (output-typed out obj))
                        ((#\~) (display #\~ out))
                        (else (raise-mismatch-error 'format-sql "unknown escape character" c))))
                  (if (= len next-start)
                      (get-output-string out)
                      (loop next-start (cdr argv)))))
            (begin (display (substring str start len) out)
                   (get-output-string out)))))))