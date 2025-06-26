
(define core:equal? #f)
(define core:assoc #f)
(define core:member #f)

(let ()
  (define (obj-equal? x y)
    (cond
      ((eq? x y) #t)
      ((and (pair? x) (pair? y))
        (list-equal? x y))
      ((and (vector? x) (vector? y))
        (vector-equal? x y))
      (else #f)))

  (define (list-equal? x y)
    (and
      (obj-equal? (car x) (car y))
      (obj-equal? (cdr x) (cdr y))))

  (define (vector-equal? x y)
    (let ((nx (vector-length x))
           (ny (vector-length y)))
      (and (eq? nx ny)
        (let loop ((i (- nx 1)))
          (or (< i 0)
            (and (obj-equal? (vector-ref x i) (vector-ref y i))
              (loop (- i 1))))))))

  (define (equal-member obj lis)
    (let loop ((obj obj))
      (cond
        ((pair? lis)
          (if (obj-equal? obj (first lis))
            lis
            (loop (rest lis))))
        ((null? lis) #f)
        (else (error "improper list at argument 2")))))

  (define (equal-assoc obj lis)
    (let loop ((lis lis))
      (cond
        ((pair? lis)
          (if (obj-equal? obj (caar lis))
            (car lis)
            (loop (cdr lis))))
        ((null? lis) #f)
        (else (error "import list at argument 2")))))

  (set! core:equal? obj-equal?)
  (set! core:member equal-member)
  (set! core:assoc equal-assoc)
)

(set! equal? core:equal?)
(set! member core:member)
(set! assoc core:assoc)