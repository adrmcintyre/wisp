(define (repl)
  (define void-value (void))

  (define (write-results . results)
    (let loop ((results results))
      (if (pair? results)
        (let ((result (first results)))
          (or (eq? void-value result)
             (begin
               (write result)
               (newline)))
          (loop (rest results))))))

  (let ((env (interaction-environment)))
    (let loop ()
      (display "repl> ")
      (with-exception-handler
        (lambda (exn)
          (display "Exception: ")
          (display exn)
          (newline)
          (loop))
        (lambda ()
          (let ((expr (read)))
            (if (not (eof-object? expr))
              (call-with-values
                (lambda() (eval expr env))
                write-results)))))
      (loop))))

(define (load filename)
  (let ((env (interaction-environment)))
    (with-input-from-file filename
      (lambda()
        (let loop ((result (void)))
          (let ((expr (read)))
            (if (eof-object? expr)
              result
              (loop (eval expr env)))))))))
