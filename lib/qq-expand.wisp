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
		(case (car x)
			((quasiquote)
				(list 'cons (list 'quote (car x)) (qq-expand (cdr x) (+ depth 1))))
			((unquote unquote-splicing)
				(cond
					((> depth 0)
							(list 'cons (list 'quote (car x)) (qq-expand (cdr x) (- depth 1))))
					((and
							(eq? 'unquote (car x))
							(not (null? (cdr x)))
							(null? (cddr x)))
						(cadr x))
					(else
						(error "Illegal"))))
			(else
				(list 'append (qq-expand-list (car x) depth)
							  (qq-expand (cdr x) depth))))
		(list 'quote x))))

(define qq-expand-list (lambda (x depth)
	(if (pair? x)
		(case (car x)
			((quasiquote)
				(list 'list (list 'cons (list 'quote (car x)) (qq-expand (cdr x) (+ depth 1)))))
			((unquote unquote-splicing)
				(cond
					((> depth 0)
						(list 'list (list 'cons (list 'quote (car x)) (qq-expand (cdr x) (- depth 1)))))
					((eq? 'unquote (car x))
						(cons 'list (cdr x))
					(else
						(cons 'append (cdr x))))))
			(else
				(list 'list (list 'append (qq-expand-list (car x) depth)
										  (qq-expand (cdr x) depth)))))
		(list 'quote (list x)))))
