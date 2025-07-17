(define-macro (do bindings test-and-result . body)
  (let ((variables (map first bindings))
         (inits (map second bindings))
         (steps (map (lambda (clause)
                       (if (null? (cddr clause))
                         (first clause)
                         (third clause)))
                  bindings))
         (test (first test-and-result))
         (result (rest test-and-result))
         (loop (gensym)))
    `(letrec ((,loop
                (lambda ,variables
                  (if ,test
                    (begin ,@result)
                    (begin
                      ,@body
                      (,loop ,@steps))))))
       (,loop ,@inits))))

