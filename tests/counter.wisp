(define tick-get 
	(lambda() (
		(lambda (n)
				(list
					(lambda() (set! n (+ 1 n)))
					(lambda() n)))
		0)))

(define t1 (tick-get)) (define tick1 (first t1)) (define get1 (second t1))
(define t2 (tick-get)) (define tick2 (first t2)) (define get2 (second t2))

(tick1)
(display "test1: ") (display (if (eq? (get1) 1) "pass" "fail")) (newline)
(display "test2: ") (display (if (eq? (get2) 0) "pass" "fail")) (newline)

