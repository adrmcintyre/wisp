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
        (qq:expand-toplevel (qq:expand (second x) 1))
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
    (if (pair? x)
        (let ((oper (first x))
              (args (rest x)))
            (if (eq? oper 'quasiquote)
                (qq:expand-qq-op oper args (+ depth 1))
                (if (or (eq? oper 'unquote)
                        (eq? oper 'unquote-splicing))
                    (if (> depth 1)
                        (qq:expand-qq-op oper args (- depth 1))
                        (if (and (eq? 'unquote oper)
                                 (not (null? args))
                                 (null? (rest args)))
                            (first args)
                            (qq:quote x)))  ;; do not throw an error here
                    (qq:expand-normal oper args depth))))
        (qq:quote x)))

; Returns the expansion of <x> within <depth> levels of quasiquote when
; in a list context.
(define (qq:expand-list x depth)
    (if (pair? x)
        (let ((oper (first x))
              (args (rest x)))
            (if (eq? oper 'quasiquote)
                (qq:list (qq:expand-qq-op oper args (+ depth 1)))
                (if (or (eq? oper 'unquote)
                        (eq? oper 'unquote-splicing))
                    (if (> depth 1)
                        (qq:list (qq:expand-qq-op oper args (- depth 1)))
                        (if (eq? oper 'unquote)
                            (qq:list-all args)
                            (if (and (pair? args) (null? (rest args)))
                                (first args)
                                (qq:append-all args))))
                    (qq:list (qq:expand-normal oper args depth)))))
        (qq:quote (list x))))

; Returns the expansion of the expression (<oper> <args> ...),
; within <depth> levels of quasiquote nesting, where <oper> is
; one of quasiquote, unquote or unquote-splicing.
(define (qq:expand-qq-op oper args depth)
    (qq:cons
        (qq:quote oper)
        (qq:expand args depth)))

; Returns the expansion of any other kind of expression.
(define (qq:expand-normal oper args depth)
    (qq:append
        (qq:expand-list oper depth)
        (qq:expand args depth)))

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
