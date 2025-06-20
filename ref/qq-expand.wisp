; vi: ft=scheme
; Adapted from http://www.r6rs.org/r6rs-editors/2006-June/001376.html

; Continue to expand until no leading quasiquote is seen.
; The referenced algorithm incorrectly only expands once.
;
; For clarity, in contrast to the referenced algorithm, we start
; <depth> at 1 so that it includes the outermost quasiquote.
(define (qq:expand-toplevel x)
    (if (and (pair? x)
             (eq? 'quasiquote (first x)))
        (if (or (null? (rest x))
                (not (null? (rest (rest x)))))
            (error "quasiquote: expects 1 argument")
            (qq:expand-toplevel
                (qq:expand (second x) 1)))
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
(define (qq:expand x depth)
    (cond
        ((symbol? x) (qq:quote x))
        ((vector? x) (qq:expand-vector x depth))
        ((pair? x)
           (let ((oper (first x)) (args (rest x)))
               (case oper
                   ((quasiquote) (qq:expand-qq-op x (+ depth 1)))
                   ((unquote unquote-splicing)
                       (if (> depth 1)
                           (qq:expand-qq-op x (- depth 1))
                           (if (and (eq? 'unquote oper)
                                    (not (null? args))
                                    (null? (rest args)))
                               (first args)
                               (qq:quote x))))  ;; do not throw an error here
                   (else (qq:expand-pair x depth)))))
        (else (qq:quote x))))

; Returns the expansion of <x> within <depth> levels of quasiquote when
; in a list context.
(define (qq:expand-list x depth)
    (cond
        ((pair? x)
            (let ((oper (first x)) (args (rest x)))
                (case oper
                    ((quasiquote) (qq:list (qq:expand-qq-op x (+ depth 1))))
                    ((unquote unquote-splicing)
                        (if (> depth 1)
                            (qq:list (qq:expand-qq-op x (- depth 1)))
                            (if (eq? oper 'unquote)
                                (qq:list-all args)
                                (if (and (pair? args)
                                         (null? (rest args)))
                                    (first args)
                                    (qq:append-all args)))))
                    (else (qq:list (qq:expand-pair x depth))))))
        (else (qq:quote (list x)))))

; Returns the expansion of the expression (<oper> <args> ...),
; within <depth> levels of quasiquote nesting, where <oper> is
; one of quasiquote, unquote or unquote-splicing.
(define (qq:expand-qq-op x depth)
    (let ((x-args (qq:expand (rest x) depth)))
        (qq:cons (qq:quote (first x)) x-args)))

; Returns the expansion of any other pair.
(define (qq:expand-pair x depth)
    (let* ((oper (first x))
           (args (rest x))
           (x-oper (qq:expand-list oper depth))
           (x-args (qq:expand args depth)))
        (qq:append x-oper x-args)))

; Returns an expression which quotes an expression:
;   <x> => '<x>
(define (qq:quote x)
    (list 'quote x))

; Returns an expression which forms a pair from the results of two expressions:
;   <x> <y> => (cons <x> <y>)
(define (qq:cons x y)
    (list 'cons x y))

; Returns an expression which forms a list from the results of a list of
; expressions:
;   (<xs> ...) => (list <xs> ...)
(define (qq:list-all xs)
    (cons 'list xs))

; Returns an expression which makes a single element list from the result of an
; expression:
;   <x> => (list <x>)
(define (qq:list x)
    (list 'list x))

; Returns an expression which appends the results of a list of expressions:
;  (<xs> ...)  => (append <xs> ...)
(define (qq:append-all xs)
    (cons 'append xs))

; Returns an expression which appends the results of two expressions:
;   <x> <y> => (append <x> <y>)
(define (qq:append x y)
    (qq:append-all (list x y)))
