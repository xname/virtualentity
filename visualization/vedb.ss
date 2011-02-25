#lang racket

;(require (planet jaz/mysql:1:7))
(require (file "mysql/main.ss"))

(provide ve-pull-new-souls
		 ve-pull-new-annotations
		 ve-pull-new-parent/soul
		 ve-pull-new-aliases
		 (struct-out soul)
		 (struct-out alias)
		 (struct-out annotation)
		 (struct-out parent/soul))

(define-struct soul (id md5 substance password filename) #:transparent)
(define-struct alias (id soul-id md5) #:transparent)
(define-struct annotation (id note soul-id-1 soul-id-2 inserted-at) #:transparent)
(define-struct parent/soul (soul-id parent-id) #:transparent)

(define ve-host "db.virtualentity.org")
(define ve-port 3306)
(define ve-user "ve")
(define ve-password "ve")
;(define ve-user "ve_gab")
;(define ve-password "ve_gab")

(define ve-connection (connect ve-host ve-port ve-user ve-password))

(query "use ve;")
;(query "use ve_gab;")

(define ve-pull-new-souls
  (let ([id->soul (make-hash)])
		(lambda ()
		  (let ([current-souls (query/map
								  (lambda (id md5 substance password filename)
									(make-soul id md5 (string->symbol substance) password filename))
								  "select id,md5,substance,password,filename from souls;")])
			(filter
			  (lambda (s)
				(if (hash-ref id->soul (soul-id s) #f)
				  #f
				  (begin
					  (hash-set! id->soul (soul-id s) s)
					  s)))
			  current-souls)))))

(define ve-pull-new-annotations
  (let ([id->annotation (make-hash)])
		(lambda ()
		  (let ([current-annotations
						(query/map
						  (lambda (id note soul-id-1 soul-id-2 inserted-at)
							(make-annotation id note soul-id-1 (if (sql-null? soul-id-2) #f soul-id-2) inserted-at))
						  "select id,note,soul_id_1,soul_id_2,inserted_at from annotations;")])
			(filter
			  (lambda (a)
				(if (hash-ref id->annotation (annotation-id a) #f)
				  #f
				  (begin
					  (hash-set! id->annotation (annotation-id a) a)
					  a)))
			  current-annotations)))))

(define ve-pull-new-parent/soul
  (let ([id->ps (make-hash)])
		(lambda ()
		  (let ([current-ps
					(query/map
					  (lambda (soul-id parent-id)
						(make-parent/soul soul-id parent-id))
					  "select soul_id,parent_soul_id from parent_soul_rel;")])
			(filter
			  (lambda (ps)
				(if (hash-ref id->ps ps #f)
				  #f
				  (begin
					  (hash-set! id->ps ps #t)
					  ps)))
			  current-ps)))))

(define ve-pull-new-aliases
  (let ([id->alias (make-hash)])
		(lambda ()
		  (let ([current-aliases
						(query/map
						  (lambda (id soul-id md5)
							(make-alias id soul-id md5))
						  "select id,soul_id,md5 from aliases;")])
			(filter
			  (lambda (a)
				(if (hash-ref id->alias (alias-id a) #f)
				  #f
				  (begin
					  (hash-set! id->alias (alias-id a) #t)
					  a)))
			  current-aliases)))))

;(close-connection! ve-connection)

