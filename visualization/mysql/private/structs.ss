#lang scheme

(require (prefix-in srfi: srfi/19)
         (only-in srfi/43 vector-map))

(provide (all-defined-out))
  
;; SQL NULL
(define-values (sql-null sql-null?)
  (let ((fn (λ ()
              (define-struct sql-null ())
              (values (make-sql-null) sql-null?))))
    (fn)))


;; DATE and TIME handling
(define current-client-timezone-offset
  (make-parameter (let ((today (srfi:current-date))) (srfi:date-zone-offset today))))

(define (parse-datetime x)
  (srfi:string->date x "~Y-~m-~d ~H:~M:~S"))

(define (parse-date x)
  (srfi:string->date x "~Y-~m-~d"))

(define (parse-time x)
  (let*-values (((neg?) (char=? (string-ref x 0) #\-))
                ((x op) (if neg? 
                            (values (substring x 1) -)
                            (values x +)))
                ((matches) (regexp-split ":" x)))
    (srfi:make-time srfi:time-duration 0 (op (+ (* 60 60 (string->number (car matches) 10)) 
                                                (* 60 (string->number (cadr matches) 10)) 
                                                (string->number (caddr matches) 10))))))

(define zero-date
  (parse-datetime "0000-00-00 00:00:00"))

(define zero-time
  (srfi:make-time 'time-duration 0 0))


;; Database objects
(define-struct connection
  (input-port 
   output-port 
   thread-id
   server-version 
   character-encoding
   (transaction #:mutable)
   (open? #:mutable)))
  
(define-struct field
  (catalog
   db
   table
   orig-table
   name
   orig-name
   character-encoding
   display-length
   type
   flags
   decimals
   default))

(define-struct query-result ())

(define-struct (result-set query-result)
  (fields 
   rows))

(define (result-set-field-names rs)
  (vector-map (λ (i f) (field-name f)) (result-set-fields rs)))

(define (in-result-set rs)
  (in-list (result-set-rows rs)))
       

(define (in-result-set/hash rs)
  (let* ((fields (result-set-fields rs))
         (keys (for/list ((f (in-vector fields)))
                         (string->symbol (field-name f))))
         (row->hash (λ (row)
                      (for/hasheq ((key (in-list keys))
                                   (value (in-vector row)))
                                  (values key value)))))
    (make-do-sequence
     (λ ()
       (values (λ (rows) (row->hash (car rows)))
               cdr
               (result-set-rows rs)
               pair?
               (λ (v) #t)
               (λ (t v) #t))))))
  

(define-struct (side-effect query-result)
  (affected-rows
   insert-id
   server-status
   warning-count
   message))
  
(define-struct prepared-statement
  (id 
   connection 
   sql 
   (sequence-count #:mutable) 
   field-count 
   parameter-count
   parameters
   types))
  
(define-struct transaction
  (level
   sp?
   prev))


(define (write-exn:mysql exn port write?)
  (let ((errno (exn:mysql-errno exn))
        (sqlstate (bytes->string/locale (exn:mysql-sqlstate exn)))
        (message (exn:mysql-message exn)))
    (if write?
        (fprintf port "#<exn:mysql errno=~s sqlstate=~s message=~s>" errno sqlstate message)
        (fprintf port "MySQL exception: errno=~a sqlstate=~a message=~a" errno sqlstate message))))
  
(define-struct exn:mysql
  (errno sqlstate message)
  #:property prop:custom-write write-exn:mysql)
