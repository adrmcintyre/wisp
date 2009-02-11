;; sourced from http://www.r6rs.org/r6rs-editors/2006-June/001376.html


; (define ev (lambda (x)
;	(eval (qq-expand-toplevel x)
;		(interaction-environment))))

(define qq-expand-toplevel (lambda (x)
	(if (eq? 'quasiquote (car x))
		(qq-expand (cadr x) 0)
		x)))

(define qq-expand (lambda (x depth)
	(if (pair? x)
		(if (eq? (car x) 'quasiquote)
			(list 'cons (list 'quote (car x)) (qq-expand (cdr x) (+ depth 1)))
			(if (or (eq? (car x) 'unquote)
				    (eq? (car x) 'unquote-splicing))
				(if (> depth 0)
					(list 'cons (list 'quote (car x)) (qq-expand (cdr x) (- depth 1)))
					(if (and
							(eq? 'unquote (car x))
							(if (null? (cdr x)) #f #t)
							(null? (cddr x)))
						(cadr x)
						(throw "Illegal")))
				(list 'append (qq-expand-list (car x) depth)
							  (qq-expand (cdr x) depth))))
		(list 'quote x))))

(define qq-expand-list (lambda (x depth)
	(if (pair? x)
		(if (eq? (car x) 'quasiquote)
			(list 'list (list 'cons (list 'quote (car x)) (qq-expand (cdr x) (+ depth 1))))
			(if (or (eq? (car x) 'unquote)
					(eq? (car x) 'unquote-splicing))
				(if (> depth 0)
					(list 'list (list 'cons (list 'quote (car x)) (qq-expand (cdr x) (- depth 1))))
					(if (eq? (car x) 'unquote)
						(cons 'list (cdr x))
						(cons 'append (cdr x))))
				(list 'list (list 'append (qq-expand-list (car x) depth)
										  (qq-expand (cdr x) depth)))))
		(list 'quote (list x)))))



(define *macro-hook* (macro (x) (qq-expand-toplevel x)))


