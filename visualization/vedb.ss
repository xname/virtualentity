#lang racket

;(require (planet jaz/mysql:1:7))
(require (file "mysql/main.ss"))

(provide souls
		 (struct-out soul)
		 aliases
		 id->alias
		 (struct-out alias)
		 annotations
		 (struct-out annotation)
		 parent-soul-rel
		 (struct-out parent/soul)
		 id->genetic)

(define-struct soul (id md5 substance password filename) #:transparent)
(define-struct alias (id soul-id md5) #:transparent)
(define-struct annotation (id note soul-id-1 soul-id-2 inserted-at) #:transparent)
(define-struct parent/soul (soul-id parent-id) #:transparent)

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

(define annotations
	(query/map
	  (lambda (id note soul-id-1 soul-id-2 inserted-at)
		(make-annotation id note soul-id-1 (if (sql-null? soul-id-2) #f soul-id-2) inserted-at))
	  "select id,note,soul_id_1,soul_id_2,inserted_at from annotations;"))

(define id->alias (make-hash))

(for-each
	(lambda (a)
	  	(let* ([id (alias-soul-id a)]
			   [num (hash-ref! id->alias id 0)])
			(hash-set! id->alias id (+ num 1))))
	aliases)

(define parent-soul-rel
	(query/map
	  (lambda (soul-id parent-id)
		(make-parent/soul soul-id parent-id))
	  "select soul_id,parent_soul_id from parent_soul_rel;"))

(define id->genetic (make-hash))
(for-each
	(lambda (ps)
		(let* ([sid (parent/soul-soul-id ps)]
			   [pid (parent/soul-parent-id ps)]
			   [num (hash-ref! id->genetic pid 0)])
			(hash-set! id->genetic pid (+ num 1))))
	parent-soul-rel)

(close-connection! ve-connection)

