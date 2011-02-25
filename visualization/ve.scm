; virtual entity visualization 

(require racket/class)
(require "vedb.ss")

(clear)
(hint-anti-alias)
(hint-depth-sort)

(define max-aliases-lum 10) ; above this the entity gets maximum luminance
(define max-genetic-size 10) ; above this the entity gets maximum size
(define damping .95)
(define simulation-speed .2)

(define (clamp v a b)
  (cond [(< v a) a]
        [(> v b) b]
        [else v]))

(define (draw-arrow p0 p1)
        (let* ([c (vnormalise (vtransform-rot #(0 0 1)
                                          (minverse (get-camera-transform))))]
               [v (vnormalise (vsub p1 p0))]
               [n (vmul (vcross v c) .1)]
               [pa (vsub p1 (vmul v .15))])

            (draw-line (vadd pa n) p1)
            (draw-line (vsub pa n) p1)
            (draw-line p0 p1)))

(define ve%
  (class object%
         (init-field s) ; soul

         (define id (soul-id s))

         (define p (build-icosphere 3))
         (define radius 
            (+ .1 (* 1 (clamp (/ (hash-ref! id->genetic id 0) max-genetic-size)
                                      0 1))))
         (define annotations 0)
         (define pos (begin
                        (random-seed (bitwise-and (string->number (soul-md5 s) 16)
                                     (sub1 (arithmetic-shift 1 31))))
                        (vmul (srndvec) 8)))
         (define ppos pos)
         (define vel (vector 0 0 0))
         (define pvel (vector 0 0 0))

         (define col 0)

         (define (calc-colour)
            (let ([c (rgb->hsv
                            (case (soul-substance s)
                                [(A) #(0 0 1)]
                                [(V) #(1 0 0)]
                                [(T) #(1 1 1)]
                                [(I) #(1 0 1)]))]
                  [nalias (hash-ref! id->alias id 0)])
              (set! col (hsv->rgb (vector (vx c) (vy c)
                                        (clamp (+ .2 (* .8 (/ nalias max-aliases-lum))) 0 1))))))

         (calc-colour)

         (define semantic '()) ; semantic relation id's
         (define genetic '()) ; genetic relation id's

         (define/public (get-radius)
            radius)

         (define/public (add-semantic other-id)
            (if other-id
                (set! semantic (cons other-id semantic))
                (set! annotations (+ 1 annotations))))

         (define/public (add-genetic other-id)
            (set! genetic (cons other-id genetic)))

         (define/public (move v)
                (set! vel (vadd vel v)))

         (define/public (update)
            (for ([other-id (append semantic genetic)])
                 (let* ([other (hash-ref ve-hash other-id)]
                        [poso (send other get-pos)]
                        [rado (send other get-radius)]
                        [dir (vsub poso pos)]
                        [d (vdist poso pos)])
                   (when (and (not (eq? other id))
                              (> d (* 5 (+ radius rado))))
                        (let ([vd (vdiv dir d)])
                          (move vd)
                          (send other move (vmul vd -1))))))
            (set! vel (vlerp pvel vel .995))
            (set! vel (vmul vel damping))
            (set! pvel vel)
            (set! pos (vadd pos (vmul vel (* .01 (delta) simulation-speed))))
            (set! pos (vlerp ppos pos .995))
            (set! ppos pos)

            (when (positive? annotations)
                (with-state
                  (hint-nozwrite)
                  (hint-unlit)
                  (colour col)
                  (opacity .5)
                  (translate (get-pos))
                  (scale (+ radius (* .05 annotations)))
                  (draw-sphere)))

            (for ([other-id semantic])
                (with-state
                  (hint-wire-stippled)
                  (wire-opacity .5)
                  (line-width 1)
                  (draw-line (get-pos) (send (hash-ref ve-hash other-id) get-pos))))
            
            (for ([other-id genetic])
                (with-state
                  (wire-opacity .5)
                  (line-width 1)
                  (let* ([p0 (get-pos)]
                         [other (hash-ref ve-hash other-id)]
                         [p1 (send other get-pos)]
                         [r (send other get-radius)]
                         [p2 (vsub p1 (vmul (vnormalise (vsub p1 p0)) r))])
                      (draw-arrow p0 p2))))

             (with-primitive p
                (identity)
                (translate pos)
                (scale radius)
                (colour col)))

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

; genetic relations
(for-each
  (lambda (s)
    (send (hash-ref ve-hash (parent/soul-parent-id s))
          add-genetic (parent/soul-soul-id s)))
  parent-soul-rel)

(define (mainloop)
  (set-camera-transform
        (mmul
            (mtranslate #(0 0 -10))
            (mrotate (vector 0 (* simulation-speed 2 (time)) 0))))
  (hash-for-each
    ve-hash
    (lambda (k v)
      (send v update))))

(every-frame
  (mainloop))

