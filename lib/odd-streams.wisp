;;; STREAMS

;; stream constructors
(define (stream-nil)
	'())

(define-macro (stream-cons x y)
	`(delay (cons ,x (delay ,y))))

;; stream predicates
(define (stream-nil? s)
	(eq? s stream-nil))

(define (stream-pair? s)
	(not (stream-nil? s)))

;; stream destructors
(define (stream-car s)
	(car (force s)))

(define (stream-cdr s)
	(force (cdr (force s))))

;; stream conversion
(define (stream->list s)
	(if (stream-nil? s) '()
		(cons (stream-car s) (stream->list (stream-cdr s)))))

(define (list->stream lis)
	(if (null? lis) stream-nil
		(stream-cons (car lis) (list->stream (cdr lis)))))

(define (stream . lis)
	(list->stream lis))

;; stream generators
(define (stream-seq m n . opt)
	(let* ((step (if (null? opt) 1 (car opt)))
		   (cmp (if (positive? step) > <)))
		(let loop ((i m))
			(if (cmp i n)
				stream-nil
				(stream-cons i (loop (+ i step)))))))

(define (stream-from m . opt)
	(let1 (step (if (null? opt) 1 (car opt)))
		(let loop ((i m))
			(stream-cons i (loop (+ i step))))))


;;; STREAM FILTERS

;; take first n elements
(define (stream-take n s)
	(if (zero? n) stream-nil
		(stream-cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;; drop first n elements
(define (stream-drop n s)
	(if (zero? n) s
		(stream-drop (- n 1) (stream-cdr s))))

;; take elements while p is true (then stop)
(define (stream-while p s)
	(let loop ((s s))
		(if (stream-nil? s) s
			(let1 (e (stream-car s))
				(if (p e)
					(stream-cons e (loop (stream-cdr s)))
					stream-nil)))))

;; take elements until p is true (then stop)
(define (stream-until p s)
	(let loop ((s s))
		(if (stream-nil? s) s
			(let1 (e (stream-car s))
				(if (p e)
					stream-nil
					(stream-cons e (loop (stream-cdr s))))))))

;; drop initial sequence of elements satisfying p
(define (stream-drop-while p s)
	(let loop ((s s))
		(if (stream-nil? s) s
			(let1 (e (stream-car s))
				(if (p e) (loop (stream-cdr s))
					s)))))

;; drop initial sequence elements not satisfying p
(define (stream-drop-until p s)
	(let loop ((s s))
		(if (stream-nil? s) s
			(let1 (e (stream-car s))
				(if (p e) s
					(loop (stream-cdr s)))))))

;; apply fn to every element
(define (stream-map fn s)
	(let loop ((s s))
		(if (stream-nil? s) s
			(stream-cons (fn (stream-car s)) (loop (stream-cdr s))))))

;; apply fn to every element
(define (stream-filter p s)
	(let loop ((s s))
		(if (stream-nil? s) s
			(let1 (e (stream-car s))
				(if (p e) (stream-cons e (loop (stream-cdr s)))
					(loop (stream-cdr s)))))))

;; return a stream of ascending integers, corresponding
;; to each element of s
(define (stream-index s)
	(let loop ((i 0) (s s))
		(if (stream-nil? s) s
			(stream-cons i (loop (+ i 1) (stream-cdr s))))))

;; return a stream of pairs of corresponding elements from s and t
(define (stream-zip s t)
	(cond
		((stream-nil? s) s)
		((stream-nil? t) t)
		(else
			(stream-cons
				(list (stream-car s) (stream-car t))
				(stream-zip (stream-cdr s) (stream-cdr t))))))

;; append 0 or more streams into a single stream
;; FIXME - not tail recursive!
(define (stream-append . ss)
	(define (stream-append2 s t)
		(let loop ((s s))
			(if (stream-nil? s) t
				(stream-cons (stream-car s) (loop (stream-cdr s))))))
	(if (null? ss) stream-nil
		(let loop ((ss ss))
			(if (null? (cdr ss)) (car ss)
				(stream-append2 (car ss) (loop (cdr ss)))))))





