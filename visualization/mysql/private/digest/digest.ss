;;; digest.ss  --  Jens Axel Søgaard

;;;
;;; PURPOSE
;;;

; This provides bindings for the message digest functions in libcrypto.
; The bindings use the high level interface as descibed in the
; libcryopto documentation:
;     <http://www.openssl.org/docs/crypto/EVP_DigestInit.html#>

; Incremental generation of the message digests are supported.

;;;
;;; SUPPORTED MESSAGE DIGEST METHODS
;;;

;   md2, md4, md5, sha, sha1, ripemd160

;;;
;;; HISTORY
;;;

; 2007-feb-03 
;   - version 1.0 released
;        bytes->hex-string
;        make-digest-context, init-context, update-context
;        final-context->bytes, final-context->hex-string
;        bytes-digest, digest, bytes-digest-port digest-port

; 2007-feb-18 
;   - version 1.1 released
;        added offsets and lengths to update-context
;        and the various named digest functions such as
;        md5, md5-bytes, etc.

; 2007-mar-10
;   - version 1.2 released
;       * applied patch from Dimitris Vyzovitis which
;         fixes bugs in the *-bytes version.
;       * added test suite


(module digest mzscheme
  (require (lib "foreign.ss")
           (all-except (lib "contract.ss") ->)
           (rename (lib "contract.ss") c-> ->))
  (unsafe!)
  
  ; open the shared library
  (define libcrypto
    (case (system-type)
      [(windows)  (ffi-lib "libeay32")]
      [else       (ffi-lib "libcrypto")]))
  
  (define hex (list->vector (string->list "0123456789abcdef")))
  
  (provide/contract [bytes->hex-string (bytes? . c-> . string?)])
  (define (bytes->hex-string bytes)
    (let* ([len (* 2 (bytes-length bytes))]
           [len/2 (quotient len 2)]
           [s (make-string len)])
      (do ([i 0 (add1 i)])
        [(= i len/2) s]
        (let* ([c    (bytes-ref bytes i)]
               [2i   (* 2 i)]
               [2i+1 (add1 2i)])
          (let-values ([(q r) (quotient/remainder c 16)])
            (string-set! s 2i   (vector-ref hex q))
            (string-set! s 2i+1 (vector-ref hex r)))))))
  
    ; hash->string : pointer integer -> string
    ;   convert the bytes pointed by the c-pointer pointer into a string.
    (define (hash->string pointer len)
      (unless (<= 0 len 64)
        (error 'hash->string 
               "the lengths of the supported message digests are between 0 and 64, got: ~a" len))
      (let* ([len (* 2 len)]
             [len/2 (quotient len 2)]
             [s (make-string len)])
        (do ([i 0 (add1 i)])
          [(= i len/2) s]
          (let* ([c    (ptr-ref pointer _byte i)]
                 [2i   (* 2 i)]
                 [2i+1 (add1 2i)])
            (let-values ([(q r) (quotient/remainder c 16)])
              (string-set! s 2i   (vector-ref hex q))
              (string-set! s 2i+1 (vector-ref hex r)))))))
  
  ;;;
  ;;; INCREMENTAL
  ;;;
  
  (define EVP_MD_CTX_create
    ; allocates, initializes and returns a digest context.
    ; EVP_MD_CTX *EVP_MD_CTX_create(void);
    (get-ffi-obj 'EVP_MD_CTX_create libcrypto
                 (_fun -> _pointer))) ; *EVP_MD_CTX
  
  (define OpenSSL_add_all_digests
    ; register all digest names for getbyname functions
    (get-ffi-obj 'OpenSSL_add_all_digests libcrypto
                 (_fun -> _void)))
  
  (define OpenSSL_add_all_ciphers
    ; register all cipher names for getbyname functions
    (get-ffi-obj 'OpenSSL_add_all_ciphers libcrypto
                 (_fun -> _void)))
  
  (OpenSSL_add_all_digests) ; must be called in order to use get-digestbyname
  ; (OpenSSL_add_all_ciphers) ; not needed in this digest only module
  
  (define EVP_get_digestbyname
    ; return an EVP_MD structure when passed a digest name
    ; !!! The digest table must be initialized using, for example, 
    ; !!! OpenSSL_add_all_digests() for this function to work.
    ; const EVP_MD *EVP_get_digestbyname(const char *name);
    (get-ffi-obj 'EVP_get_digestbyname libcrypto
                 (_fun _string -> _pointer)))
  
  (define EVP_MD_CTX_init
    ; initializes digest context ctx.
    ; void EVP_MD_CTX_init(EVP_MD_CTX *ctx);
    (get-ffi-obj 'EVP_MD_CTX_init libcrypto
                 (_fun _pointer -> _void)))
  
  (define EVP_DigestInit_ex
    ; sets up digest context ctx to use a digest type from ENGINE impl. 
    ; ctx must be initialized before calling this function. 
    ; type will typically be supplied by a functionsuch as EVP_sha1(). 
    ; If impl is NULL then the default implementation of digest type is used.
    
    ; int EVP_DigestInit_ex(EVP_MD_CTX *ctx, const EVP_MD *type, ENGINE *impl);
    (get-ffi-obj 'EVP_DigestInit_ex libcrypto
                 (_fun _pointer _pointer _pointer -> _int)))
  
  (define EVP_DigestUpdate
    ; hashes cnt bytes of data at d into the digest context ctx. 
    ; This function can be called several times on the same ctx to hash additional data.
    ; int EVP_DigestUpdate(EVP_MD_CTX *ctx, const void *d, size_t cnt);
    (get-ffi-obj 'EVP_DigestUpdate libcrypto
                 (_fun _pointer _pointer _int ;!!! TODO: is size_t always an int ?
                       -> _int)))
  
  (define EVP_DigestFinal_ex
    ; retrieves the digest value from ctx and places it in md. 
    ; If the s parameter is not NULL then the number of bytes of data written 
    ; (i.e. the length of the digest) will be written to the integer at s, 
    ; at most EVP_MAX_MD_SIZE bytes will be written. 
    ; After calling EVP_DigestFinal_ex() no additional calls to EVP_DigestUpdate() 
    ; can be made, but EVP_DigestInit_ex() can be called to initialize a new digest operation.
    
    ; int EVP_DigestFinal_ex(EVP_MD_CTX *ctx, unsigned char *md, unsigned int *s);
    (get-ffi-obj 'EVP_DigestFinal_ex libcrypto
                 (_fun _pointer _pointer _pointer -> _pointer)))
  
  (define EVP_MD_CTX_cleanup
    ; cleans up digest context ctx, it should be called after a digest context is no longer needed.
    
    ; int EVP_MD_CTX_cleanup(EVP_MD_CTX *ctx);
    (get-ffi-obj 'EVP_MD_CTX_cleanup libcrypto
                 (_fun _pointer -> _int)))
  
  ; EVP_md2(), EVP_md5(), EVP_sha(), EVP_sha1(), EVP_mdc2() and EVP_ripemd160() 
  ; return EVP_MD structures for the MD2, MD5, SHA, SHA1, MDC2 and RIPEMD160 digest algorithms respectively. 
  ; The associated signature algorithm is RSA in each case.
  ;(define EVP_md5
  ;  (get-ffi-obj 'EVP_md5 libcrypto
  ;               (_fun -> _pointer)))
  
  ; Example in C:
  ;   EVP_MD_CTX_init(&mdctx);
  ;   EVP_DigestInit_ex(&mdctx, md, NULL);
  ;   EVP_DigestUpdate(&mdctx, mess1, strlen(mess1));
  ;   EVP_DigestUpdate(&mdctx, mess2, strlen(mess2));
  ;   EVP_DigestFinal_ex(&mdctx, md_value, &md_len);
  ;   EVP_MD_CTX_cleanup(&mdctx);
  
  (define-struct context (c method md-name finalized?))
  ; A context if final-context->hex-string or final-context->bytes
  ; have been called upon it. This is only allowed once.
  
  ; the supported message digest methods
  (define md2-method       (EVP_get_digestbyname "md2"))
  (define md4-method       (EVP_get_digestbyname "md4"))
  (define md5-method       (EVP_get_digestbyname "md5"))
  (define ripemd160-method (EVP_get_digestbyname "ripemd160"))
  (define sha-method       (EVP_get_digestbyname "sha"))
  (define sha1-method      (EVP_get_digestbyname "sha1"))
  ; dss1 is the same as sha1, but dss1 must be must be used with EVP_sha1 - included only for completeness
  (define dss1-method      (EVP_get_digestbyname "dss1"))
  
  
  ; md-name->method : symbol -> pointer
  ;   return pointer to a EVP_MD structure representing a
  ;   specific message digest method.
  (define (md-name->method name)
    (case name
      [(md5) md5-method]
      [(sha) sha-method]
      [(sha1) sha1-method]
      [(md2) md2-method]
      [(md4) md4-method]
      [(ripemd160) ripemd160-method]
      [(dss1) dss1-method]
      [else (error 'md-name->method "Unknown message digest name, got: ~a" name)]))
  
  (define (md-name->output-size md-name)
    ; number of bytes in output
    (case md-name
      [(md5 md2 md4)             16]  ; 128 bits
      [(sha sha1 ripemd160 dss1) 20]  ; 160 bits
      [else (error 'md-name->output-size "Unknown message digest name, got: ~a" md-name)]))
  
  (define (make-md-context-from-method md-name method)
    (let ([c-context (EVP_MD_CTX_create)])  ; TODO: deallocation!
      (EVP_MD_CTX_init c-context)
      (EVP_DigestInit_ex c-context method #f)
      (make-context c-context method md-name #f)))
  
  
  (provide/contract [make-digest-context (symbol? . c-> . context?)])
  (define (make-digest-context md-name)
    (make-md-context-from-method md-name (md-name->method md-name)))
  
  (provide/contract [init-context (context? . c-> . void)])
  (define (init-context context)
    (let ([c-context (context-c context)])
      (EVP_MD_CTX_init c-context)
      (EVP_DigestInit_ex c-context (context-method context) #f)
      (set-context-finalized?! context #f)
      (void)))
  
  (provide/contract [update-context (case-> (context? bytes? integer? . c-> . void)
                                            (context? bytes? integer? integer? . c-> . void)
                                            (context? bytes? . c-> . void))])
  (define update-context 
    (case-lambda 
      [(context data len)
       (EVP_DigestUpdate (context-c context) data len)
       (void)]
      [(context data offset len)
       (EVP_DigestUpdate (context-c context) (ptr-add data offset) len)
       (void)]
      [(context data)
       (EVP_DigestUpdate (context-c context) data (bytes-length data))
       (void)]))
  
  (provide/contract [final-context->bytes (context? . c-> . bytes?)])
  (define (final-context->bytes context)
    (unless (not (context-finalized? context))
      (error 'final-context->bytes "A context can be finalized only once."))
    (let* ([size (md-name->output-size (context-md-name context))]
           [md (make-bytes size)]
           [c-context (context-c context)])
      (EVP_DigestFinal_ex c-context md #f)
      (set-context-finalized?! context #t)
      md))
  
  (provide/contract [final-context->hex-string (context? . c-> . string?)])
  (define (final-context->hex-string context)
    (bytes->hex-string
     (final-context->bytes context)))
  
  (provide/contract [bytes-digest (bytes? symbol? . c-> . bytes?)])
  (define (bytes-digest bytes md-name)
    (let ([context (make-digest-context md-name)])
      (update-context context bytes)
      (final-context->bytes context)))
  
  (provide/contract [digest (bytes? symbol? . c-> . string?)])
  (define (digest bytes md-name)
    (bytes->hex-string
     (bytes-digest bytes md-name)))
  
  ;;;
  ;;; INDIVIDUAL MESSAGE DIGESTS
  ;;;
  
  (define-syntax define-message-digester 
    (syntax-rules ()
      [(_ name bytes-name method-name)
       (begin
         (provide/contract 
          [name (case-> 
                 (bytes? . c-> . string?)
                 (bytes? integer? . c-> . string?)
                 (bytes? integer? integer? . c-> . string?))])
         (define name
           (case-lambda 
             [(bytes)
              (bytes->hex-string (bytes-name bytes (bytes-length bytes)))]
             [(bytes len)
              (bytes->hex-string (bytes-name bytes len))]
             [(bytes offset len)
              (bytes->hex-string (bytes-name bytes offset len))]))
         
         (provide/contract 
          [bytes-name (case-> 
                       (bytes? . c-> . bytes?)
                       (bytes? integer? . c-> . bytes?)
                       (bytes? integer? integer? . c-> . bytes?))])
         (define bytes-name
           (case-lambda 
             [(bytes)
              (bytes-name bytes (bytes-length bytes))]
             [(bytes len)
              (when (bytes? bytes)
                (unless (<= len (bytes-length bytes))
                  (error 'name "can't digest more bytes than the length of the bytes string, got ~a and ~a: "
                         bytes len)))
              (let ([context (make-md-context-from-method 'name method-name)])
                (update-context context bytes len)
                (final-context->bytes context))]
             [(bytes offset len)
              (when (bytes? bytes)
                (unless (<= (+ offset len) (bytes-length bytes))
                  (error 'name "can't digest more bytes than the length of the bytes string minus the offset, got ~a and ~a: "
                         bytes len)))
              (let ([context (make-md-context-from-method 'name method-name)])
                (update-context context (ptr-add bytes offset) len)
                (final-context->bytes context))])))]))
  
  (define-message-digester md2        md2-bytes        md2-method)
  (define-message-digester md4        md4-bytes        md4-method)
  (define-message-digester md5        md5-bytes        md5-method)
  (define-message-digester sha        sha-bytes        sha-method)
  (define-message-digester sha1       sha1-bytes       sha1-method)
  (define-message-digester ripemd160  ripemd160-bytes  ripemd160-method)
  (define-message-digester dss1       dss1-bytes       dss1-method)
  
  (provide/contract [bytes-digest-port (port? symbol? . c-> . bytes?)])
  (define (bytes-digest-port port md-name)
    (let ([context (make-digest-context md-name)])
      (let loop ()
        (let ([block (read-bytes 4096 port)])
          (cond
            [(eof-object? block) (final-context->bytes context)]
            [else                (update-context context block)
                                 (loop)])))))
  
  (provide/contract [digest-port (port? symbol? . c-> . string?)])
  (define (digest-port port md-name)
    (bytes->hex-string
     (bytes-digest-port port md-name)))
  
  )
