(define core:map #f)
(define core:for-each #f)

(let ()
	(define (improper!)
		(error "improper list"))

	; Returns #f if every obj is ()
	; Returns #t if every obj is a pair
	; Otherwise raises an - i.e. if any obj was neither () nor a pair.
	(define (continue?! objs)
		(let loop ((objs objs) (ok #t))
			(cond
				((null? objs) ok)
				((null? (first objs))
					(loop (rest objs) #f))
				((pair? (first objs))
					(loop (rest objs) ok))
				(else
					(improper!)))))

	; map over a single list.
	(define (map1 proc lis)
		(let loop ((lis lis) (result '()))
			(cond
				((pair? lis)
					(loop
						(rest lis)
						(cons (proc (first lis)) result)))
				((null? lis)
					(reverse result))
				(else
					(improper!)))))

	; map over two lists.
	(define (map2 proc lis1 lis2)
		(let loop ((lis1 lis1) (lis2 lis2) (result '()))
			(cond
				((and (pair? lis1) (pair? lis2))
					(loop
						(rest lis1)
						(rest lis2)
						(cons (proc (first lis1) (first lis2)) result)))
				((and
					 (or (pair? lis1) (null? lis1))
					 (or (pair? lis2) (null? lis2)))
					(reverse result))
				(else
					(improper!)))))

	; map over any number of lists
	(define (map-many proc lists)
		(let loop ((lists lists) (result '()))
			(if (continue?! lists)
				(loop
					(map1 rest lists)
					(cons (apply proc (map1 first lists)) result))
				(reverse result))))

	; (map proc list ...)
	;
	; Returns a list of results from applying the n-ary <proc> to the first
	; elements of each of the n <list>s, followed by the second elements, and
	; so forth.
	;
	; The result is undefined if the <list>s differ in length or are improper.
	; In this implementation, iteration stops at the end of the shortest list,
	; and it is an error if any remaining tail is neither nil nor a pair.
	(define (map proc lis . more)
		(cond
			((null? more)
				(map1 proc lis))
			((null? (rest more))
				(map2 proc lis (first more)))
			(else
				(map-many proc (cons lis more)))))

	; for-each over a single list
	(define (for-each1 proc lis)
		(let loop ((lis lis))
			(cond
				((pair? lis)
					(proc (first lis))
					(loop (rest lis)))
				((null? lis)
					(void))
				(else
					(improper!)))))

	; for-each over 2 lists
	(define (for-each2 proc lis1 lis2)
		(let loop ((lis1 lis1) (lis2 lis2))
			(cond
				((and (pair? lis1) (pair? lis2))
					(proc (first lis1) (first lis2))
					(loop (rest lis1) (rest lis2)))
				((and
					 (or (pair? lis1) (null? lis1))
					 (or (pair? lis2) (null? lis2)))
					(void))
				(else
					(improper!)))))

	; for-each over any number of lists
	(define (for-each-many proc lists)
		(let loop ((lists lists))
			(if (continue?! lists)
				(begin
					(apply proc (map1 first lists))
					(loop (map1 rest lists))))))

	; (for-each proc list ...)
	;
	; Applies the n-ary <proc> to the first elements of each of the n <list>s,
	; followed by the second elements, and so forth. No results are collected,
	; <proc> is just called for its side-effects.
	;
	; Behaviour is undefined if the <list>s differ in length or are improper.
	; In this implementation, iteration stops at the end of the shortest list,
	; and it is an error if any remaining tail is neither nil nor a pair.
	(define (for-each proc lis . more)
		(cond
			((null? more)
				(for-each1 proc lis))
			((null? (rest more))
				(for-each2 proc lis (first more)))
			(else
				(for-each-many proc (cons lis more)))))

	(set! core:map map)
	(set! core:for-each for-each)
	)

(define map core:map)
(define for-each core:for-each)