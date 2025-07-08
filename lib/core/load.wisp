
(define (core:load filename)
  (let ((env (interaction-environment)))
    (with-input-from-file filename
      (lambda()
        (let loop ((result (void)))
          (let ((expr (read)))
            (if (eof-object? expr)
              result
              (loop (eval expr env)))))))))

(set! load core:load)
