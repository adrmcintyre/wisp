
(require "srfi/1")

(define (tr x)
  (write x)
  (newline)
  x)

(define (tab-inc)
  (set! *tab-pos* (+ *tab-pos* 1)))

(define (tab-dec)
  (set! *tab-pos* (- *tab-pos* 1)))

(define (tab-reset)
  (set! *tab-pos* 0))

(define *tab-pos* 0)

(define (tab-out)
  (let loop ((t *tab-pos*))
    (if (> t 0)
      (begin
        (display " ")
        (loop (- t 1))))))

(define *traced* '())

(define (traced? sym)
  (assoc sym *traced*))

(define (add-trace sym value)
  (set! *traced* (alist-cons sym value  *traced*)))

(define (delete-trace sym)
  (set! *traced* (alist-delete sym *traced*)))

(define (make-trace sym value)
  (lambda ARGS
    (tab-out) (display "TR ") (write (cons sym ARGS)) (newline)
    (tab-inc)
    (let ((result (apply value args)))
      (tab-dec)
      (tab-out) (display "=> ") (write result) (newline)
      result)))

(define-macro (attach-trace sym)
    (if (traced? sym)
       (error "already traced")
       `(begin
         (add-trace ',sym ,sym)
         (set! ,sym (make-trace ',sym ,sym)))))

(define-macro (detach-trace sym)
    (cond
      ((traced? sym) => (lambda (sym_dot_value)
                          `(begin
                             (set! ,sym ',(cdr sym_dot_value))
                             (delete-trace ',sym))))
      (else
        (error "not traced"))))

