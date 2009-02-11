;;;
;;; promises
;;;

(define (make-promise proc)
  (let ((ready #f) 
		(result #f))
	(lambda () 
	  (if ready
		result
		(let1 (x (proc))
		  (if ready result
			(begin
			  (set! result x)
			  (set! ready #t)
			  result)))))))

(define-macro (delay x)
  `(make-promise (%lambda () ,x)))

(define (force promise)
  (promise))


