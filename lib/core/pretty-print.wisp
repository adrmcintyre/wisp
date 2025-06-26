; A basic pretty-printer.

(define core:pretty-print #f)

(let ()
  (define (pp-toplevel obj port)
    (define (put-indent indent)
      (cond
        ((< indent 8)
          (%display-atom (substring "        " 0 indent) port))
        (else
          (%display-atom "        " port)
          (put-indent (- indent 8)))))

    (define (pp-list lis indent)
      (put "(")
      (pp-obj (first lis) indent)
      (let ((next-indent (+ indent 2)))
        (let loop ((lis (rest lis)))
          (cond
            ((null? lis))
            ((pair? lis)
              (put-nl)
              (put-indent next-indent)
              (pp-obj (first lis) next-indent)
              (loop (rest lis)))
            (else
              (put " . ")
              (pp-obj lis indent)))))
      (put ")"))

    (define (pp-vector vec indent)
      (put "#(")
      (let ((n (vector-length vec)))
        (or (zero? n)
          (let ((next-indent (+ indent 2)))
            (pp-obj (vector-ref vec 0) indent)
            (do ((i 1 (+ i 1)))
              ((= i n))
              (put-nl)
              (put-indent next-indent)
              (pp-obj (vector-ref vec i) next-indent)))))
      (put ")"))

    (define (pp-obj obj indent)
      (cond
        ((pair? obj) (pp-list obj indent))
        ((vector? obj) (pp-vector obj indent))
        (else (pp-atom obj indent))))

    (define (pp-atom atom indent)
      (%write-atom atom port))

    (define (put s)
      (%display-atom s port))

    (define (put-nl)
      (newline port))

    (pp-obj obj 0)
    (put-nl)
  )

  (set! core:pretty-print
    (lambda (obj)
      (pp-toplevel obj (current-output-port))))
)

; (pretty-print obj [output-port)
; Writes a formatted representation of <obj> to the <output-port>.
; Compared to the write procedure additional whitespace and linebreaks
; are included to improve human readability.
; If <output-port> is not supplied, (current-output-port) is used instead.
;
(define pretty-print core:pretty-print)