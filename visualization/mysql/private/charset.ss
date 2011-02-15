#lang scheme
  
(require (file "structs.ss"))

(provide (all-defined-out))
  
;; Character set / encoding utilities
  
;; given MySQL's integer code for a character set/encoding/collation/whatever,
;; return a symbolic representation of the encoding 
(define (get-charset-name code)
  (case code
    ((63) 'BINARY)
    ((11 65) 'ASCII)
    ((5 8 15 31 47 48 49 94) 'ISO-8859-1)
    ((2 9 21 27 77) 'ISO-8859-2)
    ((30 78) 'ISO-8859-5)
    ((20 41 42 79) 'ISO-8859-7)
    ((1) 'BIG5)
    ((4 80) 'CP850)
    ((6 72) 'HP-ROMAN8)
    ((7 74) 'KOI8-R)
    ((12 91) 'EUC-JP)
    ((13 88) 'SHIFT_JIS)
    ((14 23 50 51 52) 'CP1251)
    ((18 89) 'TIS-620)
    ((19 85) 'EUC-KR)
    ((22 75) 'KOI8-U)
    ((26 34 66) 'CP1250)
    ((28 87) 'GBK)
    ((29 58 59 60 61) 'CP1257)
    ((32 64) 'ARMSCII-8)
    ((33 45 83) 'UTF-8)
    ((35 90) 'UCS-2)
    ((36 68) 'CP866)
    ((38 43) 'MacCentralEurope)
    ((39 53 54 55 56) 'MacRoman)
    ((40 81) 'CP852)
    (else (error "Unsupported character encoding"))))

;; converts bytes to a string, using the specified character encoding
(define (bytes->string/enc buf start end enc)
  (case enc
    ((UTF-8) (bytes->string/utf-8 buf #f start end))
    ((ISO-8859-1 BINARY) (bytes->string/latin-1 buf #f start end))
    (else
     (let*-values (((conv) (bytes-open-converter (symbol->string enc) ""))
                   ((res n status) (bytes-convert conv buf start end))
                   ((res-end status-end) (bytes-convert-end conv))
                   ((res-final) (bytes->string/locale (bytes-append res res-end))))
       (bytes-close-converter conv)
       res-final))))


;; converts a string to bytes, using the specified encoding
(define (string->bytes/enc str enc)
  (case enc
    ((UTF-8) (string->bytes/utf-8 str))
    ((ISO-8859-1) (string->bytes/latin-1 str))
    (else
     (let*-values (((buf) (string->bytes/locale str))
                   ((conv) (bytes-open-converter "" (symbol->string enc)))
                   ((res n status) (bytes-convert conv buf))
                   ((res-end status-end) (bytes-convert-end conv))
                   ((res-final) (bytes-append res res-end)))
       (bytes-close-converter conv)
       res-final))))
