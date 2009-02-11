;;
;; FIXME - this whole file is a stupid hack
;;

; need to generalise this to map n args
(define (map1 func lis)
	(let loop ((result '()) (lis lis))
		(if (null? lis)
			(reverse result)
			(loop
				(cons (func (car lis)) result)
				(cdr lis)))))

(define (map2 func lis1 lis2)
	(let loop ((result '()) (lis1 lis1) (lis2 lis2))
		(if (null? lis1)
			(reverse result)
			(loop
				(cons (func (car lis1) (car lis2)) result)
				(cdr lis1)
				(cdr lis2)))))

(define (for-each1 func lis)
	(if (pair? lis)
		(begin
			(func (car lis))
			(for-each1 func (cdr lis)))))

(define (for-each2 func lis1 lis2)
	(if (pair? lis1)
		(begin
			(func (car lis1) (car lis2))
			(for-each2 func (cdr lis1) (cdr lis2)))))

(define (map func lis1 . more)
	(if (null? more)
		(map1 func lis1)
		(map2 func lis1 (car more))))

(define (for-each func lis1 . more)
	(if (null? more)
		(for-each1 func lis1)
		(for-each2 func lis1 (car more))))

