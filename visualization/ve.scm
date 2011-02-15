; virtual entity visualization 

(require "vedb.ss")

(clear)

(define p (build-particles (length souls)))

(with-primitive p
    (pdata-map!
        (lambda (p)
            (crndvec))
        "p")
    (pdata-index-map!
        (lambda (i c)
            (case (soul-substance (list-ref souls i))
                [(A) #(0 0 1)]
                [(V) #(1 0 0)]
                [(T) #(1 1 1)]
                [(I) #(1 0 1)]))
        "c")) 
