
(define core:macro-expand #f)

(let ()
    (define *env* (interaction-environment))

    (define (expand-expr expr)
        (if (pair? expr)
            (expand-pair (first expr) (rest expr))
            expr))

    (define (expand-pair oper args)
        (let* ((argc (arg-count args))
               (x-oper (expand-expr oper)))
            (if (and (symbol? x-oper)
                     (symbol-bound? x-oper))
                (let ((proc (%eval x-oper *env*)))
                    (if (%macro? proc)
                        (expand-expr (apply proc args))
                        (expand-application x-oper args argc)))
                (expand-application x-oper args argc))))

    (define (expand-application x-oper args argc)
        (case x-oper
            ((%lambda %macro) 
                (if (< argc 2)
                    (if (< argc 1)
                        (error "missing formals")
                        (error "missing body")))
                (let ((formals (first args))
                      (body (rest args)))
                      (validate-formals formals)
                    (cons x-oper (cons formals (map expand-expr body)))))
    
            ((quote)
                (or (= argc 1)
                    (error "accepts 1 argument only"))
                (cons x-oper args))
    
            ((%define set!)
                (or (= argc 2)
                    (error "accepts 2 arguments"))
                (let ((var   (first args))
                      (value (second args)))
                    (or (symbol? var)
                        (error "argument 1 is not a symbol"))
                    (list x-oper var (expand-expr value))))
    
            ((if)
                (or (= argc 2) (= argc 3)
                    (error "accepts 2 or 3 arguments"))
                (cons x-oper (map expand-expr args)))
    
            ((and or)
                (cons x-oper (map expand-expr args)))
    
            ((begin)
                (if (= argc 1)
                    (expand-expr (first args))
                (cons x-oper (map expand-expr args))))
    
            (else
                (cons x-oper (map expand-expr args)))))
    
    (define (arg-count args)
        (let loop ((args args) (n 0))
            (cond
             ((null? args) n)
             ((pair? args) (loop (rest args) (+ n 1)))
             (else
                (error "improper argument list")))))
    
    ; Raises an error if <formals> is not valid as the formals list of a lambda
    ; expression. I.e. it must be in one of these forms:
    ;   ()
    ;   <symbol>
    ;   (<symbol> ...)
    ;   (<symbol> ... . <symbol>)
    (define (validate-formals formals)
        (cond
            ((null? formals))
            ((symbol? formals))
            ((pair? formals)
                (or (symbol? (first formals))
                    (error "formal argument is not a symbol"))
                (validate-formals (rest formals)))
            (else
                (error "rest-of-arguments formal is not a symbol"))))
    
    (set! core:macro-expand expand-expr)
)
