
(require "srfi/1")

;
; Helper functions
;

(define (debug:tab-inc)
  (set! *debug:tab-pos* (+ *debug:tab-pos* 1)))

(define (debug:tab-dec)
  (set! *debug:tab-pos* (- *debug:tab-pos* 1)))

(define (debug:tab-reset)
  (set! *debug:tab-pos* 0))

(define *debug:tab-pos* 0)

(define (debug:tab-out)
  (let loop ((t *debug:tab-pos*))
    (if (> t 0)
      (begin
        (display " ")
        (loop (- t 1))))))

(define *debug:traced* '())

(define (debug:traced? sym)
  (assoc sym *debug:traced*))

(define (debug:add-trace sym value)
  (set! *debug:traced* (alist-cons sym value  *debug:traced*)))

(define (debug:delete-trace sym)
  (set! *debug:traced* (alist-delete sym *debug:traced*)))

(define (debug:make-trace sym value)
  (lambda ARGS
    (debug:tab-out) (display "TR ") (write (cons sym ARGS)) (newline)
    (debug:tab-inc)
    (let ((result (apply value args)))
      (debug:tab-dec)
      (debug:tab-out) (display "=> ") (write result) (newline)
      result)))

;
; Enable tracing on a symbol
;
(define-macro (debug-trace sym)
    (if (debug:traced? sym)
       (error "already traced")
       `(begin
         (debug:add-trace ',sym ,sym)
         (set! ,sym (debug:make-trace ',sym ,sym)))))

;
; Disable tracing on a symbol
;
(define-macro (debug-untrace sym)
    (cond
      ((traced? sym) => (lambda (sym_dot_value)
                          `(begin
                             (set! ,sym ',(cdr sym_dot_value))
                             (debug:delete-trace ',sym))))
      (else
        (error "not traced"))))

; Output x followed by newline.
(define (tr x)
  (write x)
  (newline)
  x)

