; these *should* all return the same set of bindings - but don't...

(tronc)

(letrec (
		(f1 (lambda (x) (+ x 5)))
		(f2 (lambda (x) (* x 5)))
	)
	(list (f1 6) (f2 6)))

