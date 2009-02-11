(letrec
	(
		(both (lambda (z) (list (f1 z) (f2 z))))
		(f1 (lambda (x) (+ x 5)))
		(f2 (lambda (y) (* y 5)))
	)
	(both 6)
)

; ==>
; 
; (let ((both 0) (f1 0) (f2 0))
; 	(let (
; 			(both! (lambda (z) (list (f1 z) (f2 z))))
; 			(f1! (lambda (x) (+ x 5)))
; 			(f2! (lambda (x) (* x 5)))
; 		)
; 		(set! both both!)
; 		(set! f1 f1!)
; 		(set! f2 f2!)
; 		(both 6)))

