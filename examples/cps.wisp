(require "srfi/1")

(define fresh 
  (let ((i 0))
    (lambda (prefix)
      (set! i (+ i 1))
      (string->symbol
        (string-append
          (symbol->string prefix)
          (number->string i))))))

(define (k x) x)

(define (varref k v)
  (k v))

(define (is-varref? exp)    (symbol? exp))
(define (is-lit? exp)       (or (number? exp) (and (pair? exp) (eq? (car exp) 'quote))))
(define (is-if3? exp)       (and (eq? (car exp) 'if) (= (length exp) 4)))
(define (is-proc? exp)      (eq? (car exp) 'lambda))
(define (is-prim-app? exp)  (case (car exp) ((car cdr cons + - * / list) #t) (else #f)))
(define (is-varassign? exp) (eq? (car exp) 'set!))
(define (is-begin? exp)     (eq? (car exp) 'begin))
(define (is-app? exp)       (pair? exp))

(define-macro (cps exp)
  `(,(cps-transform exp) k))

(define (cps-transform exp)
  (cond
    ((is-varref? exp)       (cps-varref exp))
    ((is-lit? exp)          (cps-lit exp))
    ((is-if3? exp)          (cps-if3 (cadr exp) (caddr exp) (cadddr exp)))
    ((is-proc? exp)         (cps-proc (cadr exp) (caddr exp)))
    ((is-prim-app? exp)     (cps-prim-app (car exp) (cdr exp)))
    ((is-varassign? exp)    (cps-varassign (cadr exp) (caddr exp)))
    ((is-begin? exp)        (cps-begin (cdr exp)))
    ((is-app? exp)          (cps-app (car exp) (cdr exp)))
    (else (error "unexpected expression: " exp))))

(define (cps-varref var)
  (let ((k (fresh 'k)))
    `(lambda/cont(,k) (varref ,var ,k))))

(define (cps-lit datum)
  (let ((k (fresh 'k)))
    `(lambda/cont(,k) (,k ,datum))))

(define (cps-if3 test-exp then-exp else-exp)
  (let ((k (fresh 'k)))
    `(lambda/cont(,k)
       (,(cps-transform test-exp)
         ,(let ((t (fresh 't)))
            `(lambda(,t)
               (if ,t
                 (,(cps-transform then-exp) ,k)
                 (,(cps-transform else-exp) ,k))))))))

(define (cps-proc formals body)
  (let ((k (fresh 'k)))
    `(lambda/cont(,k)
       (,k
         ,(let ((k2 (fresh 'k)))
            `(lambda ,(cons k2 formals)
               (,(cps-transform body)
                 ,k2)))))))

(define (cps-prim-app prim-rator rands)
  (let ((k (fresh 'k)))
    `(lambda/cont(,k)
       ,(let loop ((rands rands) (ts '()))
          (if (null? rands)
            `(,prim-rator ,@(reverse ts) ,k)
            `(,(cps-transform (car rands))
               ,(let ((t (fresh 't)))
                  `(lambda(,t)
                     ,(loop (cdr rands) (cons t ts))))))))))

(define (cps-app rator rands)
  (let ((k (fresh 'k)))
    `(lambda/cont(,k)
       (,(cps-transform rator)
         ,(let ((f (fresh 'f)))
            `(lambda(,f)
               ,(let loop ((rands rands) (ts '()))
                  (if (null? rands)
                    `(,f ,@(reverse ts) ,k)
                    `(,(cps-transform (car rands))
                       ,(let ((t (fresh 't)))
                          `(lambda(,t)
                             ,(loop (cdr rands) (cons t ts)))))))))))))

(define (cps-varassign var exp)
  (let ((k (fresh 'k)))
    `(lambda/cont(,k)
       (,(cps-transform exp)
         ,(let ((t (fresh 't)))
            `(lambda(,t)
               (set! ,var ,t)
               (,k #f)))))))

(define (cps-begin exps)
  (let ((k (fresh 'k)))
    `(lambda/cont(,k)
       ,(if (null? exps)
          `(,k #f)
          (let loop ((exps exps))
            `(,(cps-transform (car exps))
               ,(if (null? (cdr exps))
                  k
                  (let ((_ (fresh '_)))
                    `(lambda(,_)
                       ,(loop (cdr exps)))))))))))

(define (cps-free-vars exp)
  (let loop ((exp exp) (bound '()))
    (cond
      ((pair? exp)
       (if (eq? (car exp) 'lambda)
         (let ((formals (cadr exp))
               (body (cddr exp)))
           (loop body (lset-union eq? formals bound)))
         (append
           (loop (car exp) bound)
           (loop (cdr exp) bound))))
      ((symbol? exp)
       (if (member exp bound)
         '()
         (list exp)))
      (else '()))))

(define-macro (check-args func-name . checks)
  (define (assert p)
    (or p
        (error "check-args: expects <string>, <list> of (<symbol> <symbol>) or (<any> <any> <string>)")))
  (define (make-check check)
    (assert (list? check))
    (case (length check)
      ((2)
       (let ((pred (first check))
             (var (second check)))
         (assert (and (symbol? pred) (symbol? var)))
         (let ((msg (string-append func-name ": assertion failed: " (symbol->string pred) " " (symbol->string var))))
           `(or (,pred ,var) (error ,msg)))))
      ((3)
       (let ((pred (first check))
             (var (second check))
             (msg (third check)))
         (assert (string? msg))
         (let ((msg (string-append func-name ": " msg)))
           `(or (,pred ,var) (error ,msg)))))))
  (assert (string? func-name))
  `(and . ,(map make-check checks)))

; return #t if exp is a lambda expression binding the symbol x
; supports lambdas of the form (lambda (a b c . x) ...)
(define (lambda-and-binds-var? exp x)
  (check-args
    "lambda-and-binds-var?"
    (pair? exp)
    (symbol? x))
  (and
    (eq? (first exp) 'lambda)
    (let loop ((vars (second exp)))
      (if (pair? vars)
        (or (eq? (first vars) x)
            (loop (rest vars)))
        (eq? vars x)))))

; return result of P[x:=Q]
(define (cps-subst p x q)
  (let loop ((p p))
    (cond
      ((eq? p x) q)
      ((pair? p)
       (if (lambda-and-binds-var? p x) p
         (cons (loop (car p))
               (loop (cdr p)))))
      (else p))))

;; ((lambda/cont(x) P) Q) ==> P[x:=Q]
(define (cps-beta-reduce exp)
  (cond
    ((and
       (pair? exp)
       (pair? (car exp))
       (eq? (caar exp) 'lambda/cont))
     (let ((x (caadar exp))
           (p (caddar exp))
           (q (cadr exp)))
       ; FIXME
       ; the sensible thing would be to rename all lambda variables
       ; to avoid expensive renames/lookups for clashes
       (cps-subst
         (cps-beta-reduce p)
         x
         (cps-beta-reduce q))))
    ((pair? exp)
     (cons (cps-beta-reduce (car exp))
           (cps-beta-reduce (cdr exp))))
    (else exp)))



