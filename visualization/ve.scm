; virtual entity visualization 

(require racket/class)
(require "vedb.ss")

(clear)

(define max-aliases 5) ; above this the entity gets maximum luminance

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
            (translate (vmul (crndvec) 7))
            (scale .3)
            (let ([col (rgb->hsv
                            (case (soul-substance s)
                                [(A) #(0 0 1)]
                                [(V) #(1 0 0)]
                                [(T) #(1 1 1)]
                                [(I) #(1 0 1)]))]
                  [nalias (hash-ref! id->alias (soul-id s) 0)])
              (displayln col)
              (colour (hsv->rgb (vector (vx col) (vy col)
                                        (clamp (+ .2 (* .8 (/ nalias max-aliases))) 0 1))))))

         (super-new)))

(define ve-list (map
                  (lambda (s)
                    (make-object ve% s))
                  souls))

