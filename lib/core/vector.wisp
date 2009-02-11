;;;
;;; vectors
;;;

(define (vector->list v)
  (let loop ((i (- (vector-length v) 1)) (res '()))
    (if (negative? i) res
      (loop (- i 1) (cons (vector-ref v i) res)))))

(define (list->vector elements)
  (apply vector elements))

(define (vector-fill! v k)
  (let loop ((i (- (vector-length v) 1)))
    (if (>= i 0)
      (begin
        (vector-set! v i k)
        (loop (- i 1))))))
