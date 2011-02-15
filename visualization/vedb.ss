#lang racket

;(require (planet jaz/mysql:1:7))
(require (file "mysql/main.ss"))

(provide souls
		 (struct-out soul))

(define-struct soul (id md5 substance password filename) #:transparent)

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

(close-connection! ve-connection)

