(define core:read-with-prompt #f)

(define core:read #f)

(let ()
  (define CHAR-NAMES
    '(("space" . #\space)
       ("nul" . #\nul)
       ("escape" . #\escape)
       ("rubout" . #\rubout)
       ("alarm" . #\alarm)
       ("backspace" . #\backspace)
       ("page" . #\page)
       ("newline" . #\newline)
       ("return" . #\return)
       ("tab" . #\tab)
       ("vtab" . #\vtab)))

  (define COMMA-AT (string->symbol ",@"))

  (define (string-downcase s)
    (list->string (map char-downcase (string->list s))))

  (define (string-upcase s)
    (list->string (map char-upcase (string->list s))))

  (define (identifier->keyword ident)
    (let ((n (- (string-length ident) 1)))
      (if (eq? (string-ref ident n) #\:)
        (string->keyword (substring ident 0 n))
        #f)))

  (define (identifier->object ident)
    (or
      (string->number ident)
      (let ((ident (string-downcase ident)))
        (or (identifier->keyword ident)
          (string->symbol ident)))))

  (define (read-toplevel prompt port)
    (define nesting 0)

    (define (prompt-off!)
      (set-read-prompt! #f))

    (define (prompt-on!)
      (set-read-prompt! nesting))

    (define (inc-nesting!)
      (set! nesting (+ nesting 1))
      (set-read-prompt! nesting))

    (define (dec-nesting!)
      (set! nesting (- nesting 1))
      (set-read-prompt! nesting))

    (define (read-datum)
      (read-with-token (%read-token port)))

    ; TODO - a more efficient implementation would use a 256-vector
    ; and dispatch directly on each possible initial character.
    (define (read-with-token tok)
      (if (string? tok)
        (identifier->object tok)
        (case tok
          ((#\() (read-list #\)))
          ((#\[) (read-list #\]))
          ((#\{) (read-list #\}))
          ((#\') (read-quote 'quote))
          ((#\") (read-string tok))
          ((#\#) (read-special))
          ((#\`) (read-quote 'quasiquote))
          ((#\,) (read-quote 'unquote))
          ((#\)) (error "unexpected ')'"))
          ((#\]) (error "unexpected ']'"))
          ((#\}) (error "unexpected '}'"))
          ((#\|) (read-pipe))
          (else
            (cond
              ((eq? tok COMMA-AT) (read-quote 'unquote-splicing))
              ((eof-object? tok) tok)
              (else
                (error "unknown reader syntax" tok)))))))

    (define (read-list close-paren)
      ; If we used a literal '(#f) here instead of a freshly allocated cons,
      ; when set-cdr! first modifies tail the literal itself gets changed,
      ; and future results are polluted.
      (inc-nesting!)
      (let ((head (cons #f '())))
        (let loop ((tail head))
          (let ((tok (%read-token port)))
            (cond
              ((eq? tok close-paren)
                (dec-nesting!)
                (cdr head))
              ((eq? tok #\.)
                (set-cdr! tail (read-datum))
                (let ((end-tok (%read-token port)))
                  (or (eq? end-tok close-paren)
                    (error "missing close paren" end-tok)))
                (dec-nesting!)
                (cdr head))
              ((eof-object? tok)
                (error "unexpected <eof> reading list"))
              (else
                (set-cdr! tail (cons (read-with-token tok) '()))
                (loop (cdr tail))))))))

    (define (read-vector)
      (inc-nesting!)
      (let ((head (cons #f '())))
        (let loop
          ((tail head))
          (let ((tok (%read-token port)))
            (cond
              ((eq? tok #\))
                (dec-nesting!)
                (list->vector (cdr head)))
              ((eq? tok #\.)
                (error "unexpected '.' reading vector"))
              ((eof-object? tok)
                (error "unexpected <eof> reading vector"))
              (else
                (set-cdr! tail (cons (read-with-token tok) '()))
                (loop (cdr tail))))))))

    (define (read-quote quot)
      (prompt-off!)
      (let ((datum (read-datum)))
        (prompt-on!)
        (if (eof-object? datum)
          (error "unexpected <eof> reading quoted form" quot)
          (list quot datum))))

    (define (read-string tok)
      (unread-char tok port)
      (let ((s (%read-string port)))
        (prompt-on!)
        s))

    (define (read-char-const)
      (let* ((ident (%read-identifier-string port)))
        (cond
          ((not ident) (read-char port))
          ((eof-object? ident) (error "unexpected <eof> in character constant"))
          ((= (string-length ident) 1) (string-ref ident 0))
          (else
            (let ((lookup (assoc (string-downcase ident) CHAR-NAMES)))
              (if lookup
                (cdr lookup)
                (error "unknown character constant" ident)))))))

    ; TODO - again we would profit from a vector dispatch table here,
    ; which would also open up custom reader hooks.
    (define (read-special)
      (let ((ch (read-char port)))
        (case ch
          ((#\t) #t)
          ((#\f) #f)
          ((#\\) (read-char-const))
          ((#\() (read-vector))
          ((#\i #\e #\b #\o #\d #\x) (read-special-number ch))
          (else
            (if (eof-object? ch)
              ch
              (error "unknown reader syntax" (string #\# ch)))))))

    (define (read-special-number prefix)
      (let ((suffix (%read-identifier-string port)))
        (if (eof-object? suffix)
          (error "unexpected <eof> reading number"))
        (let ((s (string-append (string #\# prefix) suffix)))
          (or (string->number s)
            (error "illegal number syntax" s)))))

    (define (read-pipe)
      (error "unknown reader syntax" "|"))

    ; read-toplevel body
    (set-read-prompt! prompt)
    (set! nesting 0)
    (read-datum)
    )

  (define (optional-port args arg caller)
    (if (null? args)
      (current-input-port)
      (if (pair? (rest args))
        (error "too many arguments" caller)
        (let ((port (first args)))
          (if (input-port? port)
            port
            (error
              (string-append "expects <input-port> at " arg)
              caller))))))

  (set! core:read-with-prompt
    (lambda (prompt . args)
      (read-toplevel prompt (optional-port args "argument 2" "core:read-with-prompt"))))

  (set! core:read
    (lambda args
      (read-toplevel #f (optional-port args "argument 1" "core:read"))))
  )

(set! read core:read)
