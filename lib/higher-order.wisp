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

(define (composelis . fns)
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
(define (hylomorphism cata ana)
	(compose cata ana))

; ... e.g.
(define hylo-factorial
	(hylomorphism (fold * 1)
		(unfold zero? id (rsect - 1))))

;
; a paramorphism
;
(define (paramorphism p f b op)
	(lambda (x)
		(let h ((x x))
			(if (p x) b
				(let ((x1 (f x)))
					(op x1 (h x1)))))))

(define para-factorial
	(paramorphism
		zero?
		(rsect - 1)
		1
		(lambda (x y) (* (+ x 1) y))))


;
; apo :: (b -> Either (c,b) [c]) -> b -> [c]
; apo f b = case f b of
;   Left (c,b) -> c : apo f b
;   Right cs -> cs

; f returns (Left c b) to insert c into the output, and recurse on b
;           (Right cs) to insert cs in the tail and stop recursing

(define (Left x) `(Left ,x))
(define (Left? x) (eq? (first x) 'Left))
(define (withLeft x fn) (fn (second x)))

(define (Right x) `(Right ,x))
(define (Right? x) (eq? (first x) 'Right))
(define (withRight x fn) (fn (second x)))

(define (Pair x y) (cons x y))
(define (withPair x fn) (fn (car x) (cdr x)))


(define-macro (deftype type . args)
	(let ((type?    (string->symbol (string-append        (symbol->string type) "?")))
		  (withType (string->symbol (string-append "with" (symbol->string type)    ))))
		`(begin
			(define (,type . ,args)
				(list ',type . ,args))
			(define (,type? x)
				(eq? (first x) ',type))
			(define (,withType x fn)
				(apply fn (rest x))))))

;
; Apomorphism is the dual of paramorphism: the point is that it is a (co)-recursion
; scheme that works with (possibly infinite) co-data (i.e. streams).
; Naturally it depends on lazy evaluation if the stream *is* infinite.
;
(define (apomorphism f b)
	(let ((fb (f b)))
		(if (Left? fb) (cons
						(withLeft fb car)
						(apomorphism f (withLeft fb cdr)))
			(withRight fb id))))

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

