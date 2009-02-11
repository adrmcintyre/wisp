(define l
	(lambda(x)
		(list
			(lambda(x)(+ x x))
			(lambda(y)(- x y))
			(lambda(z)(* x z)))))

(define p (l 5))
(define a (first p))
(define b (second p))
(define c (third p))

(display "p = ") (print p) (newline)
(display "a = ") (print a) (newline)
(display "b = ") (print b) (newline)
(display "c = ") (print c) (newline)
