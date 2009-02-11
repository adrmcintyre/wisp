(define-macro (t name test)
	`(begin
		(print ,name)
		(print ": ")
		(print (if ,test "pass" "fail"))
		(newline)))

(define-macro (f name test)
	`(begin
		(print ,name)
		(print ": ")
		(print (if ,test "fail" "pass"))
		(newline)))

(t 1 (eq? 1 1))
(f 2 (eq? 1 2))

