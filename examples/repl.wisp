(define ^ (void))
(define ^^ (void))
(define ^^^ (void))
(define ^2 (void))
(define ^3 (void))

(define (example:repl)
  (define env (interaction-environment))

  (define void-value (void))

  (define (push-result! x)
   (set! ^^^ ^^)
   (set! ^^ ^)
   (set! ^ x)
   (set! ^3 ^^^)
   (set! ^2 ^^))

  (define (receive-results . results)
    (let loop ((results results))
      (if (pair? results)
        (let ((result (first results)))
          (or (eq? result void-value)
             (begin
               (push-result! result)
               (write result)
               (newline)))
          (loop (rest results))))))

  (let loop ()
    (with-exception-handler
      (lambda (exn)
        (display "Exception: ")
        (write exn)
        (newline)
        (loop))
      (lambda ()
        (let ((expr (core:read-with-prompt "example:repl> ")))
          (if (not (eof-object? expr))
            (call-with-values
              (lambda() (eval expr env))
              receive-results)))))
    (loop)))

