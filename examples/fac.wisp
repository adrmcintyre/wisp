(run (((lambda(f) (set! f (lambda (n) (if (zero? n) 1 (* n (f (- n 1)))))) f) #f) 4))
