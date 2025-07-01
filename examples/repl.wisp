(define (repl)
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
              (let ((result (eval expr env)))
                (if (not (eq? (void) result))
                  (begin
                    (write (eval expr env))
                    (newline))))))))
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
