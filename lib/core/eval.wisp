
(define (eval expr env)
    (%eval (%compile expr) env))
