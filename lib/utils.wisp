(define-macro (rcons a b)
  `(begin
	 (set-cdr! ,a (list ,b))
	 (cdr ,a)))

