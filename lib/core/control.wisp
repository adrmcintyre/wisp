; <cond-syntax> ::= (cond <clause1> <clause2> ... [<else-clause>])
; <clause> ::= (<test>)
;          ||  (<test> <expression> ...)
;          ||  (<test> => <function>)
; <else-clause> ::= (else <expression> ...)

(define-macro (cond . clauses)
  (define (expand-cond clauses)
    (if (null? clauses) (void)
      (if (eq? (caar clauses) 'else)
        (if (null? (cdr clauses))
          `(begin ,@(cdar clauses))
          (error "cond: else in non-final position"))
        (if (null? (cdar clauses))
          (let ((sym (gensym)))
            `(let ((,sym ,(caar clauses)))
               (if ,sym ,sym
                 ,(expand-cond (cdr clauses)))))
          (if (eq? (cadar clauses) '=>)
            (let ((sym (gensym)))
              `(let ((,sym ,(caar clauses)))
                 (if ,sym
                   (,@(cddar clauses) ,sym)
                   ,(expand-cond (cdr clauses)))))
            `(if ,(caar clauses)
               (begin ,@(cdar clauses))
               ,(expand-cond (cdr clauses))))))))
  (expand-cond clauses))


; <case-syntax> ::= (case <key> <clause1> <clause2> ... [<else-clause>])
; <clause> ::= ((<datum1> ...) <expression1> <expression2> ...)
; <else-clause> ::= (else <expression1> <expression2> ...)

(define-macro (case key . clauses)
  (define (expand-case keyvar clauses)
    (let loop ((clauses clauses))
      (if (null? clauses) (void)
        (if (eq? (caar clauses) 'else)
          (if (null? (cdr clauses))
            `(begin ,@(cdar clauses))
            (error "case: else in non-final position"))
          `(if (memv ,keyvar ',(caar clauses))
             (begin ,@(cdar clauses))
             ,(loop (cdr clauses)))))))
  (let ((keyvar (gensym)))
    `(let ((,keyvar ,key))
       ,(expand-case keyvar clauses))))

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

