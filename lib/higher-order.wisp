;
; the identity function
;
(define (id x) x)


;
; argument manipulations
;
(define (lsect x op)
	(lambda (y)
		(op x y)))

(define (rsect op y)
	(lambda (x)
		(op x y)))

(define (curry fn . curried-args)
	(lambda rest-args
		(apply fn (append curried-args rest-args))))

(define (curry1 fn arg)
	(lambda rest-args
		(apply fn (cons arg rest-args))))


;
; composition
;
(define (compose f g)
	(lambda args
		(f (apply g args))))

(define (compose-list . fns)
	((foldr compose id) fns))


;
; catamorphisms (reductions)
;
(define (foldr op unit)
	(lambda (lis)
		(let loop ((lis lis))
			(if (null? lis) unit
				(op (car lis) (loop (cdr lis)))))))

(define (foldl op unit)
	(lambda (lis)
		(let loop ((lis lis) (res unit))
			(if (null? lis) res
				(loop
					(cdr lis) (op res (car lis)))))))

(define fold foldr)


;
; anamorphisms (generators)
;
(define (unfoldl p f g)
	(lambda (x)
		(if (p x) ()
			(cons (f x)
				(unfold p f g (g x))))))

(define (unfoldr p f g)
	(lambda (x)
		(let loop ((x x) (lis ()))
			(if (p x) lis
				(loop
					(g x) (cons (f x) lis))))))

(define unfold unfoldr)


;
; a hylomorphism is the composition of a catamorphism and an anamorphism...
;
(define (hylo cata ana)
	(compose cata ana))

; ... e.g.
(define hylo-factorial
	(hylo (fold * 1)
		(unfold zero? id (rsect - 1))))

;
; a paramorphism
;
(define (para p f b op)
	(lambda (x)
		(let h ((x x))
			(if (p x) b
				(let ((x1 (f x)))
					(op x1 (h x1)))))))

(define para-factorial
	(para
		zero?
		(rsect - 1)
		1
		(lambda (x y) (* (+ x 1) y))))


;
; apo :: (b -> Either (c,b) [c]) -> b -> [c]
; apo f b = case f b of
;   left (c,b) -> c : apo f b
;   right cs -> cs

; f returns (left c b) to insert c into the output, and recurse on b
;           (right cs) to insert cs in the tail and stop recursing

(define (left x) `(left ,x))
(define (left? x) (eq? (first x) 'left))
(define (with-left x fn) (fn (second x)))

(define (right x) `(right ,x))
(define (right? x) (eq? (first x) 'right))
(define (with-right x fn) (fn (second x)))

; happily pair? already exists with the correct definition
; assuming we keep pair == cons
;(define pair? pair?)
(define pair cons)
(define (with-pair x fn) (fn (car x) (cdr x)))


(define-macro (deftype type . args)
	(let ((type?     (string->symbol (string-append        (symbol->string type) "?")))
		  (with-type (string->symbol (string-append "with" (symbol->string type)    ))))
		`(begin
			(define (,type ,@args)
				(list ',type ,@args))
			(define (,type? x)
				(eq? (first x) ',type))
			(define (,with-type x fn)
				(apply fn (rest x))))))

;
; Apomorphism is the dual of paramorphism: the point is that it is a (co)-recursion
; scheme that works with (possibly infinite) co-data (i.e. streams).
; Naturally it depends on lazy evaluation if the stream *is* infinite.
;
(define (apo f b)
	(let ((fb (f b)))
		(if (left? fb) (cons
						(with-left fb car)
						(apo f (with-left fb cdr)))
			(with-right fb id))))

; what about zygomorphism and histomorphism ?


;
; zippers and unzippers
;
(define (zip lis1 lis2)
	(cond
		((null? lis1) ())
		((null? lis2) ())
		(else (cons (list (car lis1) (car lis2))
				(zip (cdr lis1) (cdr lis2))))))

(define (unzip lis)
	(let loop ((lis lis) (res1 ()) (res2 ()))
		(if (or (null? lis)
				(null? (cdr lis)))
			(list (reverse res1) (reverse res2))
			(loop
				(cddr lis)
				(cons (car lis) res1)
				(cons (cadr lis) res2)))))

