(define test-let (let ((n 1)) (list n (let ((n 2)) n))))
(define test-lambda ((lambda(n) (list n ((lambda (n) n) 2))) 1))

(display "test-let: ")    (display (if (equal? test-let    '(1 2)) "pass" "fail")) (newline)
(display "test-lambda: ") (display (if (equal? test-lambda '(1 2)) "pass" "fail")) (newline)
