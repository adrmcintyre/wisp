;;; EVEN STREAMS

;; stream helpers

(define-macro (stream-delay x)
	`(delay (force ,x)))

; NOTE it's not clear to me under what circumstances this is useful
(define-macro (stream-define proto . body)
	`(define ,proto
		(stream-delay . ,body)))

;; stream constructors
(define stream-null
	(delay '()))

(define-macro (stream-cons e s)
	`(delay (cons ,e ,s)))

;; stream predicates
(define (stream-null? s)
	(null? (force s)))

(define (stream-pair? s)
	(not (stream-null? s)))

;; stream destructors
(define (stream-car s)
	(car (force s)))

(define (stream-cdr s)
	(cdr (force s)))

;; stream conversion
(define (stream->list s)
	(if (stream-null? s) '()
		(cons
			(stream-car s)
			(stream->list (stream-cdr s)))))

;; stream generators
(define (list->stream lis)
	(if (null? lis) stream-null
		(stream-cons
			(car lis)
			(list->stream (cdr lis)))))

(define (stream . lis)
	(list->stream lis))

(define (stream-seq m n . opt)
	(let* ((step (if (null? opt) 1 (car opt)))
		   (cmp (if (negative? step) < >)))
		(let loop ((i m))
			(if (cmp i n)
				stream-null
				(stream-cons
					i
					(loop (+ i step)))))))

(define (stream-from m . opt)
	(let1 (step (if (null? opt) 1 (car opt)))
		(let loop ((i m))
			(stream-cons
				i
				(loop (+ i step))))))


;;; STREAM FILTERS

;; take first n elements
(define (stream-take n s)
	(if (zero? n) stream-null
		(stream-cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;; drop first n elements
(define (stream-drop n s)
	(if (zero? n) s
		(stream-drop (- n 1) (stream-cdr s))))

;; take elements while p is true (then stop)
(define (stream-while p s)
	(let loop ((s s))
		(if (stream-null? s) s
			(let1 (e (stream-car s))
				(if (p e)
					(stream-cons e (loop (stream-cdr s)))
					stream-null)))))

;; take elements until p is true (then stop)
(define (stream-until p s)
	(let loop ((s s))
		(if (stream-null? s) s
			(let1 (e (stream-car s))
				(if (p e)
					stream-null
					(stream-cons e (loop (stream-cdr s))))))))

;; drop initial sequence of elements satisfying p
(define (stream-drop-while p s)
	(let loop ((s s))
		(if (stream-null? s) s
			(let1 (e (stream-car s))
				(if (p e) (loop (stream-cdr s))
					s)))))

;; drop initial sequence elements not satisfying p
(define (stream-drop-until p s)
	(let loop ((s s))
		(if (stream-null? s) s
			(let1 (e (stream-car s))
				(if (p e) s
					(loop (stream-cdr s)))))))

;; apply fn to every element
(define (stream-filter p s)
	(let loop ((s s))
		(if (stream-null? s) s
			(let1 (e (stream-car s))
				(if (p e) (stream-cons e (loop (stream-cdr s)))
					(loop (stream-cdr s)))))))


;;; STREAM TRANSFORMERS

;; apply fn to every element
(define (stream-map fn s)
	(let loop ((s s))
		(if (stream-null? s) stream-null
			(stream-cons
				(fn (stream-car s))
				(loop (stream-cdr s))))))

;; return a stream of ascending integers, corresponding
;; to each element of s
(define (stream-index s)
	(let loop ((i 0) (s s))
		(if (stream-null? s) s
			(stream-cons i (loop (+ i 1) (stream-cdr s))))))

;; return a stream of pairs of corresponding elements from s and t
(define (stream-zip s t)
	(cond
		((stream-null? s) s)
		((stream-null? t) t)
		(else
			(stream-cons
				(list (stream-car s) (stream-car t))
				(stream-zip (stream-cdr s) (stream-cdr t))))))

;; append 0 or more streams into a single stream
;; FIXME - not tail recursive!
(define (stream-append . ss)
	(define (stream-append2 s t)
		(let loop ((s s))
			(if (stream-null? s) t
				(stream-cons (stream-car s) (loop (stream-cdr s))))))
	(if (null? ss) stream-null
		(let loop ((ss ss))
			(if (null? (cdr ss)) (car ss)
				(stream-append2 (car ss) (loop (cdr ss)))))))



(define (cutoff n s)
	(cond
		((zero? n) '())
		((stream-null? s) '())
		(else
			(cons
				(stream-car s)
				(cutoff (- n 1)
					(stream-cdr s))))))

