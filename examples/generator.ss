


(define-macro (let/cc k . body)
  `(call/cc
     (lambda (,k) . ,body)))

(define-macro (define-generator proto . body)
  (let ((yield (gensym))
        (cont (gensym))
        (next (gensym))
        (here (gensym)))
    (define (expand-body body)
      (if (pair? body)
        (if (eq? (first body) 'yield)
          `(call/cc
             (lambda (,cont)
               (set! ,next (lambda () (,cont 'resume)))
               (,yield . ,(rest body))))
          (cons
            (expand-body (first body))
            (expand-body (rest body))))
        body))
    (let ((expanded-body (expand-body body)))
      `(define ,proto
         (let ((,yield #f))
           (define (,next)
             ,@expanded-body
             (,yield '()))
           (lambda ()
             (call/cc
               (lambda (,here)
                 (set! ,yield ,here)
                 (,next)))))))))


(define-generator (list->generator lis)
  (let loop ((lis lis))
    (cond ((null? lis) 'skip)
          (else
            (yield (first lis))
            (loop (rest lis))))))

(define-generator (tree->generator tree)
  (let loop ((tree tree))
    (cond ((null? tree) 'skip)
          ((pair? tree)
           (loop (car tree))
           (loop (cdr tree)))
          (else (yield tree)))))

; (define (tree->generator tree)
;   (let ((yield #f))
;     (define (next)
;       (let loop ((tree tree))
;         (cond ((null? tree) 'skip)
;               ((pair? tree)
;                (loop (car tree))
;                (loop (cdr tree)))
;               (else
;                 (let/cc continue
;                         (set! next (lambda () (continue 'resume)))
;                         (yield tree)))))
;       (yield '()))
; 
;     (lambda ()
;       (let/cc here
;               (set! yield here)
;               (next)))))

(define same-fringe?
  (lambda (tree1 tree2)
    (let ((gen1 (tree->generator tree1))
          (gen2 (tree->generator tree2)))
      (let loop ()
        (let ((leaf1 (gen1))
              (leaf2 (gen2)))
          (if (eqv? leaf1 leaf2)
              (if (null? leaf1) #t (loop))
              #f))))))





  (define (div m n)
    (call/cc
      (lambda (return)
        (let ((e (call/cc
                   (lambda (throw)
                     (return
                       (if (zero? n) (throw "division by zero")
                         (/ m n)))))))
          (if (string=? e "division by zero")
            (begin (display "caught a division by zero exception!") (newline))
            (begin (display "caught an exception") (newline)))
          (return #f)))))






