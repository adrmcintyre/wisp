(define core:gcd #f)
(define core:lcm #f)

(let ()
  (define (gcd2 m n)
    (if (zero? n) m (gcd2 n (remainder m n))))

  (define (gcd . args)
    (let loop ((args args) (res 0))
      (if (null? args)
        (abs res)
        (loop
          (cdr args)
          (gcd2 (car args) res)))))

  (define (lcm . args)
    (define (lcm2 m n)
      (let ((g (gcd2 m n)))
        (cond ((= g 0) g)
          (else (* (quotient (abs m) g) n)))))
    (let loop ((args args) (res 1))
      (if (null? args)
        (abs res)
        (loop
          (cdr args)
          (lcm2 (car args) res)))))

  (set! core:gcd gcd)
  (set! core:lcm lcm)
)

(define gcd core:gcd)
(define lcm core:lcm)
