(define (backtrace1 k)        
  (let loop ((sp 0) (sf (if (continuation? k) (continuation->stack-frame k) k)))
    (cond
      (sf
        (display "[") (display sp) (display "] ")
        (display (stack-frame-pc sf))
        (let lp2 ((i 0) (n (stack-frame-length sf)))
          (cond
            ((< i n)
             (display " ")
             (display (stack-frame-ref sf i))
             (lp2 (+ i 1) n))))
        (newline)
        (loop (+ sp 1) (stack-frame-next-frame sf))))))

(define (backtrace)
  (call/cc backtrace1))

(define (fact n)
  (if (begin (backtrace) (newline) (zero? n)) 1
    (* n (fact (- n 1)))))
