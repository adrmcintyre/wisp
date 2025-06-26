
; Built-in macro expander
(define *core:macro-expander* %macro-expand)

; Uncomment to enable self-hosted macro-expansion
;
; (define *core:macro-expander*
;     (lambda (expr)
;         (core:macro-expand (core:quasiquote-expand expr))))


; Evaluation require three phases:
;   macro expansion (including expansion of quasiquote expressions)
;   compilation into a form the dispatch loop can execute
;   evaluation by dispatch loop
(define (eval expr env)
    (%eval (core:compile (*core:macro-expander* expr)) env))

