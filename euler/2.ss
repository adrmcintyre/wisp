; Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million.

; (args* -> 'a) -> (args* -> 'a)
(define (memoize f)
  (let ((memory '()))
    (lambda args
      (let ((lookup (assoc args memory)))
        (if lookup
          (cdr lookup)
          (let ((result (apply f args)))
            (set! memory (cons (cons args result) memory))
            result))))))

(define fibm
  (memoize
    (lambda (n)
      (if (< n 2) 1
        (+ (fibm (- n 1))
           (fibm (- n 2)))))))

(require "even-streams")

; (define (stream-add x y)
;   (cond ((stream-null? x) stream-null)
;         ((stream-null? y) stream-null)
;         (else
;           (stream-cons
;             (+ (stream-car x)
;                (stream-car y))
;             (stream-add
;               (stream-cdr x)
;               (stream-cdr y))))))

(define (stream-add x y)
  (if (stream-null? x) stream-null
    (if (stream-null? y) stream-null
      (stream-cons
        (+ (stream-car x)
           (stream-car y))
        (stream-add
          (stream-cdr x)
          (stream-cdr y))))))

(stream-define fibs
  (stream-cons 1
    (stream-cons 1
      (stream-add fibs (stream-cdr fibs)))))

(define (hd n s)
  (stream->list
    (stream-take n s)))

