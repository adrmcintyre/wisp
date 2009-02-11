; unfortunately this seems to be ok
(define call/cc call-with-current-continuation)
(define kk #f)
(define (inc x)
	(call/cc (lambda(k)
		(set! kk k)
		(eval
			`(if (> ,x 0)
				(+ ,x 1)
				(kk "illegal negative arg!"))
			(interaction-environment)))))

