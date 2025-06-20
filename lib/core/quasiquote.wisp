; vi: ft=scheme
; Adapted from http://www.r6rs.org/r6rs-editors/2006-June/001376.html

; Exported entry point
(define core:quasiquote-expand #f)

(let ()

    ; An unforgeable sentinel used to signify that a term has not been expanded
    ; during quasiquotation expansion, and so can be quoted as-is by the caller.
    ; It is never returned to callers of qq:expand.
    (define qq-unexpanded (gensym))

    ; Continue to expand until no leading quasiquote is seen.
    ; The referenced algorithm incorrectly only expands once.
    ;
    ; For clarity, in contrast to the referenced algorithm, we start
    ; <depth> at 1 so that it includes the outermost quasiquote.
    (define (expand-toplevel x)
        (if (and (pair? x)
                 (eq? 'quasiquote (first x)))
            (if (or (null? (rest x))
                    (not (null? (rest (rest x)))))
                (error "quasiquote: expects 1 argument")
                (expand-toplevel
                    (let* ((arg (second x))
                           (x-arg (expand arg 1)))
                        (if (eq? x-arg qq-unexpanded)
                            arg
                            x-arg))))
            x))

    ; The referenced algorithm throws an error in certain cases. In common with
    ; other implementations (e.g. guile, gambit), behave as follows instead.
    ;
    ; Allow unquote-splicing in non-list context:
    ; i.e. expand (quasiquote (unquote-splicing a)) to (quote (unquote-splicing a))
    ;
    ; Allow unquote with multiple (or zero) arguments:
    ; i.e. expand (quasiquote (unquote a b c)) to (quote (unquote a b c)) 
    ;
    ; Returns the expansion of <x> within <depth> levels of quasiquotes when
    ; in a non-list context.
    (define (expand x depth)
        (cond
            ((symbol? x) (qq-quote x))
            ((vector? x) (expand-vector x depth))
            ((pair? x)
               (let ((oper (first x)) (args (rest x)))
                   (case oper
                       ((quasiquote) (expand-qq-op x (+ depth 1)))
                       ((unquote unquote-splicing)
                           (if (> depth 1)
                               (expand-qq-op x (- depth 1))
                               (if (and (eq? 'unquote oper)
                                        (not (null? args))
                                        (null? (rest args)))
                                   (first args)
                                   (qq-quote x))))  ;; do not throw an error here
                       (else (expand-pair x depth)))))
            (else qq-unexpanded)))
    
    ; Returns the expansion of <x> within <depth> levels of quasiquote when
    ; in a list context.
    (define (expand-list x depth)
        (cond
            ((vector? x)
                (let ((x-vec (expand-vector x depth)))
                    (if (eq? x-vec qq-unexpanded)
                        qq-unexpanded
                        (qq-list x-vec))))
            ((pair? x)
                (let ((oper (first x)) (args (rest x)))
                    (case oper
                        ((quasiquote) (qq-list (expand-qq-op x (+ depth 1))))
                        ((unquote unquote-splicing)
                            (if (> depth 1)
                                (qq-list (expand-qq-op x (- depth 1)))
                                (if (eq? oper 'unquote)
                                    (qq-list-all args)
                                    (if (and (pair? args)
                                             (null? (rest args)))
                                        (first args)
                                        (qq-append-all args)))))
                        (else
                            (let ((x-expr (expand-pair x depth)))
                                (if (eq? x-expr qq-unexpanded)
                                    qq-unexpanded
                                    (qq-list x-expr)))))))
            (else qq-unexpanded)))
    
    ; Returns the expansion of the expression (<oper> <args> ...),
    ; within <depth> levels of quasiquote nesting, where <oper> is
    ; one of quasiquote, unquote or unquote-splicing.
    (define (expand-qq-op x depth)
        (let ((x-args (expand (rest x) depth)))
            (if (eq? x-args qq-unexpanded)
                (qq-quote x)
                (qq-cons (qq-quote (first x)) x-args))))
    
    ; Returns the expansion of any other pair.
    (define (expand-pair x depth)
        (let* ((oper (first x)) (x-oper (expand-list oper depth))
               (args (rest x))  (x-args (expand args depth)))
            (if (eq? x-oper qq-unexpanded)
                (if (eq? x-args qq-unexpanded)
                    qq-unexpanded
                    (qq-cons (qq-quote oper) x-args))
                (if (eq? x-args qq-unexpanded)
                    (qq-append x-oper (qq-quote args))
                    (qq-append x-oper x-args)))))
    
    ; Returns the expansion of the vector <vec> inside <depth> levels
    ; of quasiquote nesting.
    (define (expand-vector vec depth)
        (let ((x-vec (expand-pair (vector->list vec) depth)))
            (if (eq? x-vec qq-unexpanded)
                qq-unexpanded
                (list 'list->vector x-vec))))
    
    ; Returns an expression which quotes an expression:
    ;   <x> => '<x>
    (define (qq-quote x)
        (list 'quote x))
    
    ; Returns an expression which forms a pair from the results of two expressions:
    ;   <x> <y> => (cons <x> <y>)
    (define (qq-cons x y)
        (list 'cons x y))
    
    ; Returns an expression which forms a list from the results of a list of
    ; expressions:
    ;   (<xs> ...) => (list <xs> ...)
    (define (qq-list-all xs)
        (cons 'list xs))
    
    ; Returns an expression which makes a single element list from the result of an
    ; expression:
    ;   <x> => (list <x>)
    (define (qq-list x)
        (list 'list x))
    
    ; Returns an expression which appends the results of a list of expressions:
    ;  (<xs> ...)  => (append <xs> ...)
    (define (qq-append-all xs)
        (cons 'append xs))
    
    ; Returns an expression which appends the results of two expressions:
    ;   <x> <y> => (append <x> <y>)
    (define (qq-append x y)
        (qq-append-all (list x y)))

    (set! core:quasiquote-expand expand-toplevel)
)
