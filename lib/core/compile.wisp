(define core:compile #f)

; Convenience macro for destructuring a pair.
; (core:let-pair ((car-symbol . cdr-symbol) expr) body ...)
(define-macro (core:let-pair binding . body)
    (let ((a (car (first binding)))
             (d (cdr (first binding)))
             (expr (second binding))
             (value (gensym)))
        `(let ((,value ,expr))
             (let ((,a (car ,value))
                      (,d (cdr ,value)))
                 ,@body))))

(let ()
    ; must be kept in sync with values in compile.h
    (define SPECIAL-ARGC-QUOTE -1)
    (define SPECIAL-ARGC-DEFINE -2)
    (define SPECIAL-ARGC-SET-SLOT -3)
    (define SPECIAL-ARGC-SET-SYMBOL -4)
    (define SPECIAL-ARGC-IF2 -5)
    (define SPECIAL-ARGC-IF3 -6)
    (define SPECIAL-ARGC-BEGIN -7)
    (define SPECIAL-ARGC-AND -8)
    (define SPECIAL-ARGC-OR -9)
    (define SPECIAL-ARGC-APPLY -10)

    (define (compiler-error msg . extra)
        (let ((msg (string-append "during compilation: " msg)))
            (if (null? extra)
                (error msg)
                (error msg extra))))

    (define (formals-length formals)
        (let loop ((formals formals) (n 0))
            (cond
                ((null? formals) n)
                ((pair? formals) (loop (rest formals) (+ n 1)))
                (else (compiler-error "dotted argument list not allowed")))))

    ; Formals should one of these forms:
    ;   ()          returns (0 . #f)
    ;   r           returns (0 . #t)
    ;   (a b c)     returns (3 . #f)
    ;   (a b c . r) returns (3 . #t)
    (define (parse-lambda-formals formals is-macro)
        (define err-macro-arg "formal argument is not a symbol in macro definition")
        (define err-lambda-arg "formal argument is not a symbol in lambda definition")
        (define err-macro-rest "formal rest-args argument is not a symbol in macro definition")
        (define err-lambda-rest "formal rest-args argument is not a symbol in lambda definition")
        (let loop ((args formals) (argc 0))
            (if (pair? args)
                (if (symbol? (first args))
                    (loop (rest args) (+ argc 1))
                    (compiler-error (if is-macro err-macro-arg err-lambda-arg) formals))
                (cond
                    ((symbol? args) (cons argc #t))
                    ((null? args) (cons argc #f))
                    (else (compiler-error (if is-macro err-macro-rest err-lambda-rest) formals))))))

    (define (check-lambda-body body is-macro)
        (define err-macro-empty "empty macro body")
        (define err-lambda-empty "empty lambda body")
        (if (null? body)
            (compiler-error (if is-macro err-macro-empty err-lambda-empty))))

    (define (augment-env formals env depth)
        (let loop ((formals formals)
                   (argi 0)
                   (new-env '())
                   (new-depth depth))
            (cond
                ((null? formals)
                    (cons (append (reverse new-env) env) new-depth))
                ((pair? formals)
                    (let ((formal (first formals)))
                        (dup-check formal argi env)
                        (loop (rest formals) (+ argi 1) (cons formal new-env) (+ new-depth 1))))
                (else
                    (dup-check formals argi new-env)
                    (loop '() (+ argi 1) (cons formals new-env) (+ new-depth 1))))))

    ; Verifies that <symbol> is not among the first <n> entries of checklist.
    ; Raises an error if symbol is present.
    ; Returns #t if symbol is not present.
    (define (dup-check symbol n checklist)
        (let loop ((n n) (checklist checklist))
            (if (or (zero? n)
                    (null? checklist))
                #t
                (if (eq? symbol (car checklist))
                    (compiler-error "repeated identitifer" symbol)
                    (loop (- n 1) (rest checklist))))))

    ; returns (symbol-or-slot . new-max-slot)
    (define (compile-symbol symbol env depth max-slot)
        (let loop ((env env) (depth (- depth 1)))
            (cond
                ((null? env)
                    (cons symbol max-slot))
                ((eq? (first env) symbol)
                    (cons
                        (if (< depth 0) symbol (%make-slot depth))
                        (max max-slot depth)))
                (else
                    (loop (rest env) (- depth 1))))))

    (define (compile-body body env depth max-slot)
        (let loop ((body body) (max-slot max-slot) (c-body '()))
            (if (null? body)
                (cons (reverse c-body) max-slot)
                (core:let-pair ((c-expr . max-slot) (compile-with-env (first body) env depth max-slot))
                    (loop (rest body) max-slot (cons c-expr c-body))))))

    (define (compile-quote argc args env depth max-slot)
        (or (= argc 1)
            (compiler-error "quote: accepts 1 argument only"))
        (cons
            (cons SPECIAL-ARGC-QUOTE args)
            max-slot))

    (define (compile-define argc args env depth max-slot)
        (or (= argc 2)
            (compiler-error "define: accepts 2 arguments"))
        (let ((symbol (first args))
              (value (second args)))
            (or (symbol? symbol)
                (compiler-error "define: 1st argument is not a symbol"))
            (core:let-pair ((c-symbol . max-slot) (compile-symbol symbol env depth max-slot))
                (if (%slot? c-symbol)
                    (compiler-error "define: 1st argument is lexically bound"))
                (core:let-pair ((c-value . max-slot) (compile-with-env value env depth max-slot))
                    (cons
                        (list SPECIAL-ARGC-DEFINE c-symbol c-value)
                        max-slot)))))

    (define (compile-set! argc args env depth max-slot)
        (or (= argc 2)
            (compiler-error "set!: accepts 2 arguments"))
        (let ((symbol (first args))
              (value (second args)))
            (or (symbol? symbol)
                (error "set!: 1st argument is not a symbol"))
            (core:let-pair ((c-symbol . max-slot) (compile-symbol symbol env depth max-slot))
                (core:let-pair ((c-value . max-slot) (compile-with-env value env depth max-slot))
                    (cons
                        (list
                            (if (%slot? c-symbol) SPECIAL-ARGC-SET-SLOT SPECIAL-ARGC-SET-SYMBOL)
                            c-symbol
                            c-value)
                        max-slot)))))

    (define (compile-special special-argc args env depth max-slot)
        (core:let-pair ((c-body . max-slot) (compile-body args env depth max-slot))
            (cons
                (cons special-argc c-body)
                max-slot)))

    (define (compile-if argc args env depth max-slot)
        (compile-special
            (case argc
                ((2) SPECIAL-ARGC-IF2)
                ((3) SPECIAL-ARGC-IF3)
                (else (compiler-error "if: accepts 2 or 3 arguments only")))
            args env depth max-slot))

    (define (compile-lambda argc args env depth max-slot)
        (compile-lambda-or-macro argc args env depth max-slot #f))

    (define (compile-macro argc args env depth max-slot)
        (compile-lambda-or-macro argc args env depth max-slot #t))

    (define (compile-lambda-or-macro argc args env depth max-slot is-macro)
        (define err-bad-lambda "ill-formed lambda expression")
        (define err-bad-macro "ill-formed macro expression")
        (or (>= argc 2)
            (compiler-error (if is-macro err-bad-macro err-bad-lambda)))
        (let ((formals (first args))
              (body (rest args)))
            (core:let-pair ((formals-argc . want-rest) (parse-lambda-formals formals is-macro))
                (check-lambda-body body is-macro)
                (core:let-pair ((new-env . new-depth) (augment-env formals env depth))
                    (core:let-pair ((c-body . max-slot) (compile-body body new-env new-depth max-slot))
                        (cons
                            (%make-compiled-lambda is-macro formals-argc want-rest max-slot depth c-body)
                            max-slot))))))

    (define (compile-with-env expr env depth max-slot)
        (cond
            ((symbol? expr) (compile-symbol expr env depth max-slot))
            ((pair? expr) (compile-pair expr env depth max-slot))
            (else (cons expr max-slot))))

    (define (compile-pair expr env depth max-slot)
        (let* ((oper (first expr))
               (args (rest expr))
               (argc (formals-length args)))
            (core:let-pair ((c-oper . max-slot) (compile-with-env oper env depth max-slot))
                (case c-oper
                    ((%lambda)
                        (compile-lambda argc args env depth max-slot))
                    ((%macro)
                        (compile-macro argc args env depth max-slot))
                    ((quote)
                        (compile-quote argc args env depth max-slot))
                    ((%define)
                        (compile-define argc args env depth max-slot))
                    ((set!)
                        (compile-set! argc args env depth max-slot))
                    ((if)
                        (compile-if argc args env depth max-slot))
                    ((begin)
                        (compile-special SPECIAL-ARGC-BEGIN args env depth max-slot))
                    ((and)
                        (compile-special SPECIAL-ARGC-AND args env depth max-slot))
                    ((or)
                        (compile-special SPECIAL-ARGC-OR args env depth max-slot))
                    (else
                        (compile-application c-oper argc args env depth max-slot))))))

    (define (compile-application c-oper argc args env depth max-slot)
        (core:let-pair ((c-body . max-slot) (compile-body args env depth max-slot))
            (cons (cons argc (cons c-oper c-body))
                max-slot)))

    (define (compile-toplevel expr)
        (core:let-pair ((c-expr . max-slot) (compile-with-env expr '() 0 0))
            c-expr))

    (set! core:compile compile-toplevel)
)
