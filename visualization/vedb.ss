#lang racket

;(require (planet jaz/mysql:1:7))
(require (file "mysql/main.ss"))

(provide souls
		 (struct-out soul)
		 aliases
		 id->alias
		 (struct-out alias))

(define-struct soul (id md5 substance password filename) #:transparent)
(define-struct alias (id soul-id md5) #:transparent)

(define ve-host "db.virtualentity.org")
(define ve-port 3306)
(define ve-user "ve")
(define ve-password "ve")

(define ve-connection (connect ve-host ve-port ve-user ve-password))

(query "use ve;")

(define souls
	(query/map
	  (lambda (id md5 substance password filename)
		(make-soul id md5 (string->symbol substance) password filename))
	  "select id,md5,substance,password,filename from souls;"))

(define aliases
	(query/map
	  (lambda (id soul-id md5)
		(make-alias id soul-id md5))
	  "select id,soul_id,md5 from aliases;"))

(define id->alias (make-hash))

(for-each
	(lambda (a)
	  	(let* ([id (alias-soul-id a)]
			   [num (hash-ref! id->alias id 0)])
			(hash-set! id->alias id (+ num 1))))
	aliases)

(close-connection! ve-connection)

