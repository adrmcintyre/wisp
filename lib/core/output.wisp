
; The builtin display and write functions consume unbounded stack in the
; c runtime. Here we replace them with scheme versions.

(define core:display #f)
(define core:write #f)

(let ()
  (define SPECIALS-TABLE
    '((quote . "'")
       (unquote . ",")
       (unquote-splicing . ",@")
       (quasiquote . "`")))

  (define (special-prefix form)
    (and
      (pair? (cdr form))
      (null? (cddr form))
      (let ((prefix (assq (first form) SPECIALS-TABLE)))
        (and prefix (cdr prefix)))))

  (define (special-body form)
    (second form))

  (define (optional-port args caller)
    (if (null? args)
      (current-output-port)
      (if (pair? (rest args))
        (error "too many arguments" caller)
        (let ((port (first args)))
          (if (output-port? port)
            port
            (error "expects <output-port> at argument 2" caller))))))

  (define (output-toplevel obj out port)
    (define (output obj)
      (cond
        ((pair? obj) (output-list obj))
        ((vector? obj) (output-vector obj))
        (else (output-atom obj))))

    (define (output-list lis)
      (let ((prefix (special-prefix lis)))
        (if prefix
          (output-special prefix (special-body lis))
          (output-unspecial-list lis))))

    (define (output-special prefix body)
      (put prefix)
      (output body))

    (define (output-unspecial-list lis)
      (put "(")
      (output (car lis))
      (let loop ((lis (rest lis)))
        (cond
          ((null? lis)
            (put ")"))
          ((pair? lis)
            (put " ")
            (output (car lis))
            (loop (cdr lis)))
          (else
            (put " . ")
            (output lis)
            (put ")")))))

    (define (output-vector vec)
      (put "#(")
      (let ((n (vector-length vec)))
        (if (> n 0)
          (begin
            (output (vector-ref vec 0))
            (do ((i 1 (+ i 1)))
              ((= i n))
              (put " ")
              (output (vector-ref vec i)))))
        (put ")")))

    (define (output-atom atom)
      (out atom port))

    (define (put s)
      (%display-atom s port))

    (output obj))

  (set! core:write
    (lambda (obj . opt)
      (output-toplevel obj %write-atom (optional-port opt 'write))))

  (set! core:display
    (lambda (obj . opt)
      (output-toplevel obj %display-atom (optional-port opt 'display))))
)

(define display core:display)
(define write core:write)
