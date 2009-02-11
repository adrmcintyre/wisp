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
(define (is-if? exp)        (eq? (car exp) 'if))
(define (is-proc? exp)      (eq? (car exp) 'proc))
(define (is-prim-app? exp)  (case (car exp) ((car cdr cons + - * / list) #t) (else #f)))
(define (is-varassign? exp) (eq? (car exp) ':=))
(define (is-begin? exp)     (eq? (car exp) 'begin))
(define (is-let? exp)       (eq? (car exp) 'let))
(define (is-letrec? exp)    (eq? (car exp) 'letrec))
(define (is-app? exp)       (pair? exp))

(define-macro (cps exp)
  `(,(cps-transform exp) k))

(define (cps-transform exp)
  (cond
    ((is-varref? exp) (cps-varref exp))
    ((is-lit? exp)    (cps-lit exp))
    ((is-if? exp)     (cps-if (cadr exp) (caddr exp) (cadddr exp)))
    ((is-proc? exp)   (cps-proc (cadr exp) (caddr exp)))
    ((is-prim-app? exp) (cps-prim-app (car exp) (cdr exp)))
    ((is-varassign? exp) (cps-varassign (cadr exp) (caddr exp)))
    ((is-begin? exp)  (cps-begin (cdr exp)))
    ((is-let? exp)    (cps-let (cadr exp) (caddr exp)))
    ((is-letrec? exp) (cps-letrec (cadr exp) (caddr exp)))
    ((is-app? exp)    (cps-app (car exp) (cdr exp)))
    (else (error "unexpected expression: " exp))))

(define (cps-varref var)
  (let ((k (fresh 'k)))
    `(kappa(,k) (varref ,k ,var))))

(define (cps-lit datum)
  (let ((k (fresh 'k)))
    `(kappa(,k) (,k ,datum))))

(define (cps-if test-exp then-exp else-exp)
  (let ((k (fresh 'k)))
    `(kappa(,k)
       (,(cps-transform test-exp)
         ,(let ((t (fresh 't)))
            `(lambda(,t)
               (if ,t
                 (,(cps-transform then-exp) ,k)
                 (,(cps-transform else-exp) ,k))))))))

(define (cps-proc formals body)
  (let ((k (fresh 'k)))
    `(kappa(,k)
       (,k
         ,(let ((k2 (fresh 'k)))
            `(lambda ,(cons k2 formals)
               (,(cps-transform body)
                 ,k2)))))))

(define (cps-prim-app prim-rator rands)
  (let ((k (fresh 'k)))
    `(kappa(,k)
       ,(let loop ((rands rands) (ts '()))
          (if (null? rands)
            `(,prim-rator ,k ,@(reverse ts))
            `(,(cps-transform (car rands))
               ,(let ((t (fresh 't)))
                  `(lambda(,t)
                     ,(loop (cdr rands) (cons t ts))))))))))

(define (cps-app rator rands)
  (let ((k (fresh 'k)))
    `(kappa(,k)
       (,(cps-transform rator)
         ,(let ((f (fresh 'f)))
            `(lambda(,f)
               ,(let loop ((rands rands) (ts '()))
                  (if (null? rands)
                    `(,f ,k ,@(reverse ts))
                    `(,(cps-transform (car rands))
                       ,(let ((t (fresh 't)))
                          `(lambda(,t)
                             ,(loop (cdr rands) (cons t ts)))))))))))))

(define (cps-varassign var exp)
  (let ((k (fresh 'k)))
    `(kappa(,k)
       (,(cps-transform exp)
         ,(let ((t (fresh 't)))
            `(lambda(,t)
               (set! ,var ,t)
               (,k #f)))))))

(define (cps-begin exps)
  (let ((k (fresh 'k)))
    `(kappa(,k)
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

(define (check-args func-name . checks)
  (define (make-check check)
    (define (assert p)
      (or p
          (error "check-args: expects (<symbol> <symbol>) or (<any> <any> <string>)")))
    (assert (list? check))
    (case (length check)
      ((2)
       (let ((pred (first check))
             (var (second check)))
         (assert (and (symbol? pred) (symbol? var)))
         (let ((msg (string-append func-name ": assertion failed: " (symbol->string pred) " " (symbol->string var))))
           `(if (not (,pred ,var)) (error ,msg)))))
      ((3)
       (let ((pred (first check))
             (var (second check))
             (msg (third check)))
         (assert (string? msg))
         `(if (not (,pred ,var)) (error ,msg))))))
  `(begin . ,(map make-check checks)))

(define (lambda-and-binds-var? exp x)
  (check-args
    "lambda-and-binds-var?"
    (exp pair?)
    (x symbol?))
  (and
    (eq? (car exp) 'lambda)
    (let loop ((vars (cadr exp)))
      (or
        (eq? vars x)
        (eq? (car vars) x)
        (loop (cdr vars))))))

(define (cps-subst p x q)
  (let loop ((p p))
    (cond
      ((eq? p x) q)
      ((pair? p)
       (if (lambda-and-binds-var? p x) p
         (cons (loop (car p))
               (loop (cdr p)))))
      (else p))))

;; ((kappa(x) P) Q) ==> P[x:=Q]
(define (cps-beta-reduce exp)
  (cond
    ((and
       (pair? exp)
       (pair? (car exp))
       (eq? (caar exp) 'kappa))
     (let ((x (caadar exp))
           (p (caddar exp))
           (q (cadr exp)))
       (cps-subst
         (cps-beta-reduce p)
         x
         (cps-beta-reduce q))))
    ((pair? exp)
     (cons (cps-beta-reduce (car exp))
           (cps-beta-reduce (cdr exp))))
    (else exp)))

