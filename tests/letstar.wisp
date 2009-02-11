(let*
	(
		(f1 (lambda (x) (+ x 5)))
		(f2 (lambda (y) (* y 5)))
		(both (lambda (z) (list (f1 z) (f2 z))))
	)
	(both 6)
)
