(define (hefty-computation do-other-stuff)
	(let loop ((n 5))
		(display "Hefty computation: ")
		(display n)
		(newline)
		(set! do-other-stuff (call/cc do-other-stuff))
		(display "Hefty computation (b)")
		(newline)
		(set! do-other-stuff (call/cc do-other-stuff))
		(display "Hefty computation (c)")
		(newline)
		(set! do-other-stuff (call/cc do-other-stuff))
		(if (> n 0)
			(loop (- n 1)))))

(define (superfluous-computation do-other-stuff)
	(let loop ()
		(for-each
			(lambda (graphic)
				(display graphic)
				(newline)
				(set! do-other-stuff (call/cc do-other-stuff)))
			'(
				"Straight up."
				"Quarter after."
				"Half past."
				"Quarter til."))
		(loop)))

(hefty-computation superfluous-computation)
