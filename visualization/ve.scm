; virtual entity visualization 

(require racket/class)
(require "vedb.ss")

(clear)
(hint-anti-alias)

(define max-aliases-lum 5) ; above this the entity gets maximum luminance
(define max-genetic-size 5) ; above this the entity gets maximum size

(define (clamp v a b)
  (cond [(< v a) a]
        [(> v b) b]
        [else v]))

(define ve%
  (class object%
         (init-field s) ; soul

         (define p (build-icosphere 3))
         (with-primitive p
            (random-seed (bitwise-and (string->number (soul-md5 s) 16)
                         (sub1 (arithmetic-shift 1 31))))
            (translate (vmul (srndvec) 8))
            (scale (+ .2 (* 2 (clamp (/ (hash-ref! id->genetic (soul-id s) 0) max-genetic-size)
                                      0 1))))
            (let ([col (rgb->hsv
                            (case (soul-substance s)
                                [(A) #(0 0 1)]
                                [(V) #(1 0 0)]
                                [(T) #(1 1 1)]
                                [(I) #(1 0 1)]))]
                  [nalias (hash-ref! id->alias (soul-id s) 0)])
              (colour (hsv->rgb (vector (vx col) (vy col)
                                        (clamp (+ .2 (* .8 (/ nalias max-aliases-lum))) 0 1))))))

         (define semantic '()) ; semantic relation id's

         (define/public (add-semantic other-id)
            (when other-id
                (set! semantic (cons other-id semantic))))

         (define/public (update)
            (for ([other-id semantic])
                (with-state
                  (wire-opacity .5)
                  (line-width 2)
                  (draw-line (get-pos) (send (hash-ref ve-hash other-id) get-pos)))))

         (define/public (get-pos)
            (let ([t (with-primitive p
                            (get-transform))])
                  (vector (vector-ref t 12) (vector-ref t 13) (vector-ref t 14))))

         (super-new)))

(define ve-hash (make-hash))

; setup souls
(for-each
  (lambda (s)
    (hash-set! ve-hash (soul-id s) (make-object ve% s)))
  souls)

; semantic relations
(for-each
  (lambda (a)
    (send (hash-ref ve-hash (annotation-soul-id-1 a))
          add-semantic (annotation-soul-id-2 a)))
  annotations)

(define (mainloop)
  (hash-for-each
    ve-hash
    (lambda (k v)
      (send v update))))

(every-frame
  (mainloop))


