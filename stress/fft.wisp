(define-macro (when test true-body . rest)
  `(if ,test (begin ,true-body ,@rest)))

(define-macro (unless test false-body . rest)
  `(if ,test (void) (begin ,false-body ,@rest)))

;------------------------------------------------

(define *PI* 3.141592653589793)

(define (four1 data fwd)
  (let ((n (vector-length data))
         (pi*2 (* (if fwd 2. -2.) *PI*)))

    ;; bit-reversal section

    (let loop1 ((i 0) (j 0))
      (when (< i n)
        (when (< i j)
          (let ((temp (vector-ref data i)))
            (vector-set! data i (vector-ref data j))
            (vector-set! data j temp))
          (let ((temp (vector-ref data (+ i 1))))
            (vector-set! data (+ i 1) (vector-ref data (+ j 1)))
            (vector-set! data (+ j 1) temp)))
        (let loop2 ((m (quotient n 2)) (j j))
          (if (and (>= m 2) (>= j m))
            (loop2 (quotient m 2) (- j m))
            (loop1 (+ i 2) (+ j m))))))

    ;; Danielson-Lanczos section

    (let loop3 ((mmax 2))
      (when (< mmax n)
        (let* ((theta
                 (/ pi*2 (inexact mmax)))
                (wpr
                  (let ((x (sin (* 0.5 theta))))
                    (* -2.0 (* x x))))
                (wpi
                  (sin theta)))
          (let loop4 ((wr 1.0) (wi 0.0) (m 0))
            (when (< m mmax)
              (let loop5 ((i m))
                (if (< i n)
                  (let* ((j
                           (+ i mmax))
                          (tempr
                            (-
                              (* wr (vector-ref data j))
                              (* wi (vector-ref data (+ j 1)))))
                          (tempi
                            (+
                              (* wr (vector-ref data (+ j 1)))
                              (* wi (vector-ref data j)))))
                    (vector-set! data j
                      (- (vector-ref data i) tempr))
                    (vector-set! data (+ j 1)
                      (- (vector-ref data (+ i 1)) tempi))
                    (vector-set! data i
                      (+ (vector-ref data i) tempr))
                    (vector-set! data (+ i 1)
                      (+ (vector-ref data (+ i 1)) tempi))
                    (loop5 (+ j mmax)));***))
                  (loop4 (+ (- (* wr wpr) (* wi wpi)) wr)
                    (+ (+ (* wi wpr) (* wr wpi)) wi)
                    (+ m 2))))
              ));******
          (loop3 (* mmax 2)))))))

(define data
  (make-vector 1024 0.0))

(define (run data)
  (four1 data #t)
  (vector-ref data 0))
