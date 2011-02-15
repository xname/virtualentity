#lang scheme

(require (file "private/structs.ss")
         (file "private/protocol.ss")
         (file "private/charset.ss")
         (file "private/packet-decoders.ss")
         (file "private/session-features.ss")
         (file "private/auth.ss")
         (file "private/io.ss")
         (only-in srfi/43 reverse-list->vector)
         openssl)

(provide current-connection
         
         connect
         close-connection!
         connection?
         connection-open?
         
         query*
         query
         query0
         
         query/foldl
         query/map
         query/map/filter
         
         prepare
         execute
         execute/foldl
         execute/map
         execute/map/filter
         
         call-with-transaction
         with-transaction
         
         sql-null
         sql-null?
         
         prepared-statement?
         prepared-statement-parameter-count
         (struct-out result-set)
         result-set-field-names
         (struct-out side-effect)
         (struct-out field)
         
         in-result-set
         in-result-set/hash
         )

;; current-connection : () -> (U connection? #f)
;;                    : (connection?) -> void?
(define current-connection (make-parameter #f))

;; Connects to the database.  Returns a connection object (a struct)
;; connect : (string? integer? string? string? #:schema string? #:use-ssl? boolean? #:set-current? boolean?) -> connection?
(define (connect host port user password 
                 #:schema (schema #f)
                 #:use-ssl? (use-ssl? #f)
                 #:set-current? (set-current? #t))
                 
  (when (and use-ssl? (not ssl-available?))
    (error "Cannot use SSL, because openssl libraries are not available/usable on this machine."))
  
  (let*-values (((ip op) (tcp-connect host port))
                
                ((len seq packet) (read-packet ip))
                ((protocol-version server-version thread-id salt1 salt2 features encoding status)
                 (decode-handshake packet 0))
                
                ((server-features) (integer->features features))
                ((client-features) (if schema
                                       (cons 'connect-with-db default-client-features)
                                       default-client-features))
                ((version) (parse-server-version server-version))
                ;; 45 is 4-byte UTF-8, whereas 33 is 3-byte UTF-8
                ((charset-byte) (if (server-version>=? version 6 0 0) 45 33)))

    (check-protocol-version protocol-version)

    (let*-values (((ip op client-features next-seq) 
                   (cond ((and use-ssl? (memq 'ssl server-features))
                          (let* ((client-features (cons 'ssl client-features))
                                 (packet (make-client-auth-packet client-features charset-byte #f #f #f)))
                            ;; send the addreviated packet
                            (send-packet 1 packet op)
                            ;; then switch to SSL
                            (let-values (((ip op) (ports->ssl-ports ip op '#:mode 'connect '#:encrypt 'tls '#:close-original? #t)))
                              (values ip op client-features 2))))
                         (use-ssl?
                          (error "MySQL server does not support SSL connections."))
                         (else (values ip op client-features 1))))
                  
                  ((pw-hash) (scramble-411 (string->bytes/utf-8 password) (bytes-append salt1 salt2)))
                  ((auth-packet) (make-client-auth-packet client-features charset-byte user pw-hash schema)))
      
      (send-packet next-seq auth-packet op)

      (let-values (((len seq buf) (check-error-packet ip 'UTF-8)))
        (when (eof-packet? buf) ;; EOF: fallback on old hash algorithm
          (send-packet (add1 seq) (make-retro-password-bytes (string->bytes/utf-8 password) salt1) op)
          (check-error-packet ip 'UTF-8))
        
        (let ((con (make-connection ip op thread-id version 'UTF-8 #f #t)))
          (when set-current?
            (current-connection con))
          con)))))


;; close a connection
;; close-connection! : connection? -> void?
(define (close-connection! con)
  (when (not (connection? con))
    (raise-type-error 'close-connection! "MySQL connection" con))

  (when (connection-open? con)
    (send-packet 0 (bytes QUIT) (connection-output-port con))
    (close-input-port (connection-input-port con))
    (close-output-port (connection-output-port con))
    (set-connection-open?! con #f)))

(define-syntax with-connection-handler
  (syntax-rules ()
    ((_ con exp)
     (with-handlers ((exn:fail? (λ (e)
                                  (when (connection-open? con)
                                    (with-handlers ((exn:fail? (λ (e-ignore) #t)))
                                      (close-connection! con)))
                                  (raise e))))
       exp))))


;; utilities for building query results
;;
(define (build-result-set fields rows/reverse)
  (make-result-set fields (reverse rows/reverse)))

(define (kons-row/vector row rows)
  (cons (reverse-list->vector row) rows))

(define (build/foldl fields val)
  val)

(define (make-kons-row/foldl proc)
  (λ (revlst acc)
    (apply proc (reverse (cons acc revlst)))))

(define (build/map fields vals/reverse)
  (reverse vals/reverse))

(define (make-kons-row/map proc)
  (λ (revlst vals)
    (cons (apply proc (reverse revlst)) vals)))

(define (make-kons-row/map/filter proc pred?)
  (λ (revlst vals)
    (let ((lst (reverse revlst)))
      (if (apply pred? lst)
          (cons (apply proc lst) vals)
          vals))))

;; Query procedures
;;

;; query*-engine (connection? string? ((vectorof field?) 'a -> 'b) ((vectorof any/c) 'a -> 'a) 'a -> (listof (or/c side-effect? 'b))) 
(define (query*-engine connection sql build-result kons-row knil)
  (with-connection-handler connection
    (let ((enc (connection-character-encoding connection))
          (ip (connection-input-port connection))
          (op (connection-output-port connection)))
      (send-packet 0 (bytes-append (bytes QUERY) (string->bytes/enc sql enc)) op)
      
      (let loop ((res '()) (more? #t))
        (cond (more?
               (let-values (((len seq buf) (check-error-packet ip enc)))
                 (case (bytes-ref buf 0)
                   ((#x00) (let-values (((field-count affected-rows insert-id server-status warning-count message) (decode-ok buf 0 enc)))
                             (loop (cons (make-side-effect affected-rows (and (positive? insert-id) insert-id)
                                                           server-status warning-count message) res)
                                   (more-results? server-status))))
                   (else (let*-values (((field-count extra) (decode-result-set-header buf 0))
                                       ((fields) (decode-field-packet-sequence ip field-count enc))
                                       ((rows more?) (decode-text-row-packet-sequence ip field-count fields kons-row knil)))
                           (loop (cons (build-result fields rows) res)
                                 more?))))))
              (else res))))))





;; query* : (#:connection connection? string?) -> (listof? (or/c result-set? side-effect?))
(define (query* #:connection (connection (current-connection)) sql)
  (reverse (query*-engine connection sql build-result-set kons-row/vector '())))

;; query : (#:connection connection? string?) -> (or/c result-set? side-effect?)
(define (query #:connection (connection (current-connection)) sql)
  (car (query*-engine connection sql build-result-set kons-row/vector '())))

;; query0 : (#:connection connection? string?) -> (or/c result-set? side-effect?)
(define (query0 #:connection (connection (current-connection)) sql)
  (car (query* #:connection connection sql)))

;; query/foldl (#:connection connection? (any/c ... 'a -> 'a) 'a string? -> (or/c side-effect? 'a))
(define (query/foldl #:connection (connection (current-connection)) proc init sql)
  (car (query*-engine connection sql build/foldl (make-kons-row/foldl proc) init)))

;; query/map (#:connection connection? (any/c ... -> 'a) string? -> (or/c side-effect? (listof 'a)))
(define (query/map #:connection (connection (current-connection)) proc sql)
  (car (query*-engine connection sql build/map (make-kons-row/map proc) '())))

;; query/map/filter (#:connection connection? (any/c ... -> 'a) (any/c ... -> boolean?) string? -> (or/c side-effect? (listof 'a)))
(define (query/map/filter #:connection (connection (current-connection)) proc pred? sql)
  (car (query*-engine connection sql build/map (make-kons-row/map/filter proc pred?) '())))

;; prepare : (#:connection connection? string?) -> prepared-statement?
(define (prepare #:connection (connection (current-connection)) sql)
  (with-connection-handler connection
    (let ((enc (connection-character-encoding connection))
          (ip (connection-input-port connection))
          (op (connection-output-port connection)))
      (send-packet 0 (bytes-append (bytes PREPARE) (string->bytes/enc sql enc)) op)
    
      (let*-values (((len seq buf) 
                     (check-error-packet ip enc))
                  
                    ((field-count stmt-id num-cols num-params) 
                     (decode-prep-init buf 0))
                    
                    ((num-param-packets num-col-packets) 
                     (values (if (positive? num-params) (add1 num-params) 0)
                             (if (positive? num-cols) (add1 num-cols) 0)))
                    
                    ((params) 
                     (if (positive? num-params)
                         (decode-field-packet-sequence ip num-params enc)
                         '#())))

        ;; throw away the column packets
        (let loop ((i num-col-packets))
          (when (positive? i)
            (read-packet ip)
            (loop (sub1 i))))
        
        (make-prepared-statement stmt-id connection sql 0;(+ seq num-param-packets num-col-packets 1) 
                                 num-cols num-params params (make-vector num-params #f))))))

;; close-statement! : (prepared-statement? -> void?)
(define (close-statement! stmt)
  (when (not (prepared-statement? stmt))
    (raise-type-error 'close-statement! "prepared-statement?" stmt))
  
  (let* ((con (prepared-statement-connection stmt))
         (op (connection-output-port con))
         (ip (connection-input-port con))
         (enc (connection-character-encoding con)))
    (send-packet 0 (bytes-append (bytes STMT-CLOSE) (integer->integer-bytes (prepared-statement-id stmt) 4 #f #f)) op)
    (void)))

;; execute-engine : (prepared-statement? list? ((vectorof field?) 'a -> 'b) ((vectorof any/c) 'a -> 'a) 'a) -> (or/c side-effect? 'b)
(define (execute-engine stmt args build-result kons-row knil)
  (when (not (= (length args) (prepared-statement-parameter-count stmt)))
    (raise-mismatch-error 'execute "number of arguments doesn't match the number of parameters in the prepared statement" args))
  
  (let ((connection (prepared-statement-connection stmt)))
    (with-connection-handler connection
      (let*-values (((enc ip op) (values (connection-character-encoding connection)
                                         (connection-input-port connection)
                                         (connection-output-port connection)))
                    ((reset? send-types? arg-bytes null-bitmap long-params) (encode-prepared-statement-arguments stmt args)))
        ;; reset the statement if necessary
        (when reset?
          (set-prepared-statement-sequence-count! stmt 0)
          (send-packet 0 (bytes-append (bytes STMT-RESET) (integer->integer-bytes (prepared-statement-id stmt) 4 #f #f)) op)
          (check-error-packet ip enc))
    
        ;; send whatever long data we have
        (let loop ((lps long-params))
          (when (pair? lps)
            (let ((p (car lps)))
              (send-long-parameter stmt (car p) (cdr p))
              (loop (cdr lps)))))

        ;; assemble and send the EXEC packet
        (let ((packet (bytes-append (bytes STMT-EXEC)
                                    (integer->integer-bytes (prepared-statement-id stmt) 4 #f #f)
                                    (bytes 0        ;; flags
                                           1 0 0 0) ;; iteration count
                                    null-bitmap
                                    (get-parameter-type-bytes stmt send-types?)
                                    arg-bytes)))
          (set-prepared-statement-sequence-count! stmt 0)
          (send-packet 0 packet op)

          ;; get the query results
          (let-values (((len seq buf) (check-error-packet ip enc)))
            (case (bytes-ref buf 0)
              ((#x00) (let-values (((field-count affected-rows insert-id server-status warning-count message) (decode-ok buf 0 enc)))
                        (make-side-effect affected-rows (and (positive? insert-id) insert-id) server-status warning-count message)))
              (else (let*-values (((field-count extra) (decode-result-set-header buf 0))
                                  ((fields) (decode-field-packet-sequence ip field-count enc))
                                  ((rows) (decode-binary-row-packet-sequence ip field-count fields kons-row knil)))
                      (build-result fields rows))))))))))
                                    
;; execute : (prepared-statement? list? -> (or/c side-effect? result-set?))  
(define (execute stmt args)
  (execute-engine stmt args build-result-set kons-row/vector '()))

;; execute/foldl : ((any/c ... 'a -> 'a) 'a prepared-statement? list? -> (or/c side-effect? 'a))
(define (execute/foldl proc init stmt args)
  (execute-engine stmt args build/foldl (make-kons-row/foldl proc) init))

;; execute/map : ((any/c ... -> 'a) prepared-statement? list? -> (or/c side-effect? (listof? 'a)))
(define (execute/map proc stmt args)
  (execute-engine stmt args build/map (make-kons-row/map proc) '()))

;; execute/map/filter : ((any/c ... -> 'a) (any/c ... -> boolean?) prepared-statement? list? -> (or/c side-effect? (listof? 'a)))
(define (execute/map/filter proc pred? stmt args)
  (execute-engine stmt args build/map (make-kons-row/map/filter proc pred?) '()))


;; Transaction utilities
;;

(define (transaction-begin connection sp?)
  (let* ((cur (connection-transaction connection))
         (level (if cur (add1 (transaction-level cur)) 0))
         (xa (make-transaction level sp? cur)))
    
    (if (zero? level)
        (query #:connection connection "BEGIN WORK")
        (and sp? (query #:connection connection (format "SAVEPOINT _sp_~a_~a" (connection-thread-id connection) level))))

    (set-connection-transaction! connection xa)
    xa))

(define (transaction-commit connection xa)
  (let ((level (transaction-level xa)))
    (if (zero? level)
        (query #:connection connection "COMMIT")
        (and (transaction-sp? xa)
             (query #:connection connection (format "RELEASE SAVEPOINT _sp_~a_~a" (connection-thread-id connection) level)))))
    
  (set-connection-transaction! connection (transaction-prev xa)))

(define (transaction-rollback connection xa)    
  (let ((level (transaction-level xa)))
    (if (zero? level)
        (query #:connection connection "ROLLBACK")
        (and (transaction-sp? xa)
             (query #:connection connection (format "ROLLBACK TO _sp_~a_~a" (connection-thread-id connection) level)))))
    
  (set-connection-transaction! connection (transaction-prev xa)))
  

(define (isolation-level-name iso)
  (case iso
    ((repeatable-read) "REPEATABLE READ")
    ((read-uncommitted) "READ UNCOMMITTED")
    ((read-committed) "READ COMMITTED")
    ((serializable) "SERIALIZABLE")
    (else (error (format "Unknown transaction isolation level: ~a" iso)))))


(define (call-with-transaction proc
          #:connection (connection (current-connection))
          #:isolation (isolation #f)
          #:use-savepoints? (use-savepoints? #f))
  (let/ec k
    (when (and isolation (not (connection-transaction connection)))
      (query #:connection connection (format "SET TRANSACTION ISOLATION LEVEL ~a" (isolation-level-name isolation))))

    (let ((xa (transaction-begin connection use-savepoints?)))
    
      (with-handlers ((exn:mysql? (λ (e) (transaction-rollback connection xa) (raise e))))
        (let* ((fail (λ (v) (transaction-rollback connection xa) (k v)))
               (res (proc fail)))
          (transaction-commit connection xa)
          res)))))


(define-syntax with-transaction
  (syntax-rules ()
    ((_ (con iso use-sp?) (rollback) exp ...)
     (call-with-transaction (λ (escape)
                              (let ((rollback (λ (v) (escape v))))
                                exp ...))
           #:connection con
           #:isolation iso
           #:use-savepoints? use-sp?))
    ((_ (iso use-sp?) (rollback) exp ...)
     (with-transaction ((current-connection) iso use-sp?) (rollback) exp ...))
    ((_ (iso) (rollback) exp ...)
     (with-transaction ((current-connection) iso #f) (rollback) exp ...))
    ((_ () (rollback) exp ...)
     (with-transaction ((current-connection) #f #f) (rollback) exp ...))))
