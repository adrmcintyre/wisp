(define (ev0 exp env)
  (cond
    ((symbol? exp) (env-get env exp))
    ((pair? exp)
     (let ((func (first exp))
           (args (rest exp)))
       (case func
         ((lambda) (list (cadr exp) (cddr exp) env))
         ((if) (if (truish? (ev0 (first args) env))
                 (ev0 (second args) env)
                 (ev0 (third args) env)))
         ((set!) (env-set! env (first args) (ev0 (second args) env)))
         ((define) (let ((var (first args)) (val (ev0 (second args) env)))
                     (cond ((assoc var env) => (lambda(binding) (set-cdr! binding val)))
                           (else (set! env (cons (cons var val) env))))))
         ((begin) (ev-body args env))
         ((quote) (first args))
         ((call/cc eval apply) (raise-error "unimplemented: " func))
         (else
           (applicate (ev0 func env) (ev-list args env))))))
    (else exp)))

(define (ev-list exp-list env)
  (if (null? exp-list) '()
    (cons (ev0 (first exp-list) env)
          (ev-list (rest exp-list) env))))

(define (ev-body exp-list env)
  (let ((evhead (ev0 (car exp-list) env))
        (tail (cdr exp-list)))
    (if (null? tail) evhead
      (ev-body tail env))))

(define (truish? val)
  (if val #t #f))

(define (applicate func args)
  (if (pair? func)
    (let ((formals (first func))
          (body (second func))
          (env (third func)))
      (ev-body body (env-extend env formals args)))
    ;; defer to host implementation
    (apply func args)))
    
(define (env-extend env formals args)
  (cond
    ((null? formals)
     (if (null? args) env
       (raise-error "too many arguments for lambda")))
    ((pair? formals)
     (if (null? args)
       (raise-error "too few arguments for lambda")
       (env-extend (cons (cons (first formals) (first args)) env)
                   (rest formals)
                   (rest args))))
    (else
      (cons (cons formals args) env))))

(define (env-get env var)
  (cond
    ((assoc var env) => cdr)
    (else (raise-error "undefined variable: " var))))

(define (env-set! env var val)
  (cond
    ((assoc var env) => (lambda (binding) (set-cdr! binding val)))
    (else (raise-error "undefined variable: " var))))

(define (raise-error . args)
  (error
    (apply string-append
           (map (lambda (x)
                  (cond ((string? x) x)
                        ((symbol? x) (symbol->string x))
                        ((number? x) (number->string x))
                        (else (error "bad args"))))
                args))))

(define-macro (build-env . syms)
  (cons 'list
        (let loop ((syms syms))
          (if (null? syms) '()
            (cons
              `(cons ',(car syms) ,(car syms))
              (loop (cdr syms)))))))

(define core-env
  (build-env
    gc-info gc-check gc booleans->integer list->integer integer->list reverse-bit-field 
    rotate-bit-field ash arithmetic-shift copy-bit-field bit-field copy-bit logbit? bit-set? 
    log2-binary-factors first-set-bit integer-length logcount bit-count logtest any-bits-set? 
    bitwise-if bitwise-merge lognot bitwise-not logxor bitwise-xor logior bitwise-ior logand 
    bitwise-and file-stat stack-frame-ref stack-frame-next-frame stack-frame-env stack-frame-pc 
    stack-frame-length continuation->stack-frame stack-frame? continuation? %mysql:close 
    %mysql:escape %mysql:free %mysql:row %mysql:fields %mysql:query %mysql:connect error trace-load 
    load write-char newline display pretty-print write char-ready? eof-object? peek-char read-char 
    read close-output-port close-input-port open-output-file open-input-file 
    %set-current-output-port! %set-current-input-port! current-output-port current-input-port 
    output-port? input-port? procedure? not boolean? string->keyword keyword->string keyword? 
    gensym string->symbol symbol->string symbol? char-downcase char-upcase integer->char 
    char->integer char-lower-case? char-upper-case? char-whitespace? char-numeric? char-alphabetic? 
    char-ci>=? char-ci<=? char-ci>? char-ci<? char-ci=? char>=? char<=? char>? char<? char=? char? 
    string-ci>=? string-ci<=? string-ci>? string-ci<? string-ci=? string>=? string<=? string>? 
    string<? string=? string-fill! string-copy list->string string->list string-append substring 
    string-set! string-ref string-length string make-string string? equal? eq? eqv? %record-set! 
    %record-ref %record %make-record record? vector-set! vector-ref vector-length vector 
    make-vector vector? string->number number->string / * - + > >= != = <= < max min expt atan sqrt 
    acos asin tan cos sin log exp round truncate ceiling floor modulo remainder quotient abs even? 
    odd? positive? negative? zero? inexact? exact? integer? rational? real? complex? number? assoc 
    assv assq member memv memq list-ref list-tail reverse append length list list? null? set-cdr! 
    set-car! cddddr cdddar cddadr cddaar cdaddr cdadar cdaadr cdaaar cadddr caddar cadadr cadaar 
    caaddr caadar caaadr caaaar third cdddr cddar cdadr cdaar caddr cadar caadr caaar second cddr 
    cdar cadr caar rest first cdr car cons pair? unquote-splicing unquote quasiquote
    undefined void trace-eval apply %compile trace-compile))

(define (ev exp)
  (ev0 exp core-env))
  
