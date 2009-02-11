
(define *cc-cfile*   "/tmp/wisp-cc.c")

(define *cc-outfile* "/tmp/wisp-cc.out")

(define *cc-prologue* "
    #include <stdio.h>
    int main() {
        printf(\"%f\\n\", (float)(
")

(define *cc-epilogue* "
        ));
    }
")

(define (initial-env) '())

(define *cc-initial-env* (initial-env))

(define (env-lookup env sym)
  (let ((binding (assoc sym env)))
    (if binding (cdr binding)
      (error (string-append "unbound symbol: " (symbol->string sym))))))

(define (cc expr)
  (let ((text (cc-main expr)))
    (with-output-to-file *cc-cfile* (lambda () (display text)))
    (display text)
    (newline)
    (if (zero? (system (string-append "gcc -o " *cc-outfile* " " *cc-cfile*)))
      (system *cc-outfile*)
      (error "compile failed"))
    (void)))

(define (cc-main expr)
  (string-append
    *cc-prologue*
    (cc-expr *cc-initial-env* expr)
    *cc-epilogue*))

(define (let-expr? expr)
  (and
    (pair? expr)
    (eq? (first expr) 'let)))

; (sqrt
;   (let ((a (* 3 3))
;         (b (* 4 4)))
;     (+ a b)))
; =>
;   a = 3 * 3;
;   b = 4 * 4;
;   _1 = a + b;
;   _2 = sqrt(_1);
;
(define (cc-let env expr)
  (let ((bindings (first expr))
        (sub-expr (second expr)))


(define (cc-expr env expr)
  (cond
    ((let-expr? expr)
     (cc-let env expr))
    ((pair? expr)
     (string-append
       "("
       (cc-op env (first expr) (rest expr))
       ")"))
    ((number? expr)
     (cc-number env expr))
    ((symbol? expr)
     (cc-symbol env expr))
    (else
      (error "bad literal"))))

(define (cc-symbol env sym)
  (env-lookup env sym))

(define (cc-op env op args)
  (let ((n (length args)))
    (case op
      ((+) (cc-fold env "+" "0" args))
      ((*) (cc-fold env "*" "1" args))
      ((-) (if (= n 1) (cc-negate env (first args))
             (cc-fold env "-" "0" args)))
      ((/) (if (= n 1) (cc-reciprocal env (first args))
             (cc-fold env "/" "1" args)))
      (else (error "bad operator")))))

(define (cc-number env num)
  (number->string num))

(define (cc-fold env op unit args)
  (let loop ((rev (reverse args)))
    (if (null? rev) unit
      (if (null? (rest rev)) (cc-expr env (first rev))
        (string-append
          (loop (rest rev))
          op
          (cc-expr env (first rev)))))))

(define (cc-negate env arg)
  (string-append "-" (cc-expr env arg)))

(define (cc-reciprocal env arg)
  (string-append "1/" (cc-expr env arg)))


