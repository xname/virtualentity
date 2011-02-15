#lang setup/infotab

(define name "MySQL Client Library")

(define blurb
  '("The MySQL Client Library provides access to MySQL databases via TCP."))

(define primary-file "main.ss")

(define categories '(net))

(define can-be-loaded-with 'all)

(define repositories '("4.x"))

(define scribblings '(("scribblings/manual.scrbl" (multi-page))))

(define release-notes '((p "Version 1.7: VARBINARY column data in prepared statements was being returned as character strings. Now, it is correctly returned as byte strings. (Thanks to Yinso Chen for the bug report.)")))
