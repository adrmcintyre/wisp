; Virtual instructions so far:
;   (label LABEL)				-- declare LABEL
;   (lambda-header LABEL N V)   -- declare a lambda with N fixed args, and V = #t if accepts variable args
;   (branch-if-false LABEL)     -- if !value then pc = LABEL
;   (branch LABEL)				-- pc = LABEL
;   (lit VALUE)					-- value = VALUE
;   (push-context)              -- push env, pc
;   (jump ARG-COUNT)
;       -- call function in value, with ARG-COUNT args from stack
;       -- if value contains a builtin, copy the args into a frame, call it, and leave the result in value
;       -- if value contains a closure, copy the args into a frame
;
;   (declare-global SYM)		-- reserve space in global symbol table if needed
;   (set-global! SYM)			-- *locate_global(SYM) = value
;   (get-global SYM)			-- value = *locate_global(SYM)
;	(set-slot! FRAME SLOT)		-- *locate_slot(FRAME,SLOT) = value
;   (get-slot FRAME SLOT)		-- value = *locate_slot(FRAME,SLOT)
;   (make-closure LABEL)		-- value = make_closure(LABEL,env)
;   (return)					-- pop env, pc
;   (void)						-- value = void
;
; Registers:
;   value
;   env
;   pc
;   sp


; env is a list of ((symbol ...) (symbol ...) ...)

(define *global-env* '())

; (define (assemble explist)
;   (


; leaves result in value
(define (cc:ev0 exp env)
  (cond
    ((symbol? exp) (cc:env-get exp env))
    ((pair? exp)
     (let ((func (first exp))
           (args (rest exp)))
       (case func
         ((lambda) (cc:lambda (cadr exp) (cddr exp) env))
         ((if)
			(case (length args)
				((2) (cc:if2 (first args) (second args) env))
				((3) (cc:if3 (first args) (second args) (third args) env))
				(else (raise-error "if needs either 2 or 3 arguments"))))
         ((set!) (cc:set! (first args) (second args) env))
         ((define) (cc:define (first args) (second args) env))
         ((begin) (cc:begin args env))
         ((quote) (cc:literal (first args)))
         ((call/cc eval apply) (raise-error "unimplemented: " func))
         (else
           (cc:application func args env)))))
    (else (cc:literal exp))))

(define (length-fixed-args args)
  (if (pair? args)
    (+ 1 (length-fixed-args (rest args)))
    0))

(define (var-args? args)
  (if (pair? args)
    (var-args? (rest args))
    (not (null? args))))

(define (env-extend args env)
  (cons
    (let loop ((args args))
      (if (pair? args)
        (cons (first args) (loop (rest args)))
        (list args)))
    env))

(define (make-slot frame offset name)
  (list frame offset name))

(define slot-frame  first)
(define slot-offset second)
(define slot-name   third)

(define (env-lookup arg env)
  (let next-frame ((frame 0) (env env))
    (and
      (pair? env)
      (let next-slot ((offset 0) (frame-vars (first env)))
        (if (pair? frame-vars)
          (if (eq? arg (first frame-vars))
            (make-slot frame offset arg)
            (next-slot (+ 1 offset) (rest frame-vars)))
          (next-frame (+ 1 frame) (rest env)))))))

;; FIXME - really just a placeholder at this stage
;; we need to figure out how this should interact with cc:application
;; (i.e. this is the really hard bit!)

; leaves result in value
(define (cc:lambda args body env)
  (let ((lambda-label (gen-label))
        (skip-label (gen-label)))
    `(
        (lambda-header ,lambda-label ,(length-fixed-args args) ,(var-args? args) ,args)
        ,@(cc:begin body (env-extend args env))
        (return)
		(make-closure ,lambda-label)
     )))

; leaves result in value
(define (cc:application func args env)
  `(
    ,@(cc:ev-args args env)
    ,@(cc:ev0 func env)
    (push-context)
    (jump ,(length args))
  ))

; pushes all results to stack
; trashes value
(define (cc:ev-args args env)
  (if (null? args) '()
    `(
      ,@(cc:ev0 (first args) env)
      (push)
      ,@(cc:ev-args (rest args) env)
    )))

(define gen-label
  (let ((label 0))
    (lambda ()
      (set! label (+ label 1))
      (string->symbol (string-append "L" (number->string label))))))

; leaves void in value
(define (cc:if2 test true-branch env)
  (let ((else-label (gen-label))
        (endif-label (gen-label)))
	  `(
		  ,@(cc:ev0 test env)
		  (branch-if-false ,endif-label)
		  ,@(cc:ev0 true-branch env)
		  (label ,endif-label)
          (void)
      )))

; leaves result in value
(define (cc:if3 test true-branch false-branch env)
  (let ((else-label (gen-label))
        (endif-label (gen-label)))
	  `(
		  ,@(cc:ev0 test env)
		  (branch-if-false ,else-label)
		  ,@(cc:ev0 true-branch env)
		  (branch ,endif-label)
		  (label ,else-label)
		  ,@(cc:ev0 false-branch env)
		  (label ,endif-label)
      )))

; leaves result in value
(define (cc:begin body env)
  (case (length body)
    ((0) '(void))
    ((1) (cc:ev0 (first body) env))
    (else
	  `(
		,@(cc:ev0 (first body) env)
		,@(cc:begin (rest body) env)
      ))))

; leaves result in value
(define (cc:literal arg)
  `(
    (lit ,arg)
  ))

; leaves void in value
(define (cc:define var value env)
  `(
    ,@(cc:ev0 value env)
    ,@(cc:set-global! var)
  ))

; leaves void in value
(define (cc:set! var value env)
  `(
    ,@(cc:ev0 value env)
    ,@(cond
        ((env-lookup var env) => cc:set-slot!)
        (else (cc:set-global! var)))
    (void)))

; leaves void in value
(define (cc:set-slot! slot)
  `(
    (set-slot! ,(slot-frame slot) ,(slot-offset slot) ,(slot-name slot))
    (void)
  ))

; leaves void in value
(define (cc:set-global! var)
  `(
    (set-global! ,@(get-global-offset var))
    (void)
  ))

; (define (get-global-offset var)
;   var)

(define get-global-offset
  (let ((index 0))
    (lambda (var)
      (cond
        ((assoc var *global-env*) => rest)
        (else
          (set! index (+ index 1))
          (let ((offset (list index var)))
            (set! *global-env* (cons (cons var offset) *global-env*))
            offset))))))

; leaves result in value
(define (cc:env-get var env)
  (cond
    ((env-lookup var env) => cc:get-slot)
    (else (cc:get-global var))))

; leaves result in value
(define (cc:get-slot slot)
  `(
     (get-slot ,(slot-frame slot) ,(slot-offset slot) ,(slot-name slot))
  ))

; leaves result in value
(define (cc:get-global var)
  `(
     (get-global ,@(get-global-offset var))
  ))

(define (raise-error . args)
  (error
    (apply string-append
           (map (lambda (x)
                  (cond ((string? x) x)
                        ((symbol? x) (symbol->string x))
                        ((number? x) (number->string x))
                        (else (error "bad args"))))
                args))))

; (define-macro (build-env . syms)
;   (cons 'list
;         (let loop ((syms syms))
;           (if (null? syms) '()
;             (cons
;               `(cons ',(car syms) ,(car syms))
;               (loop (cdr syms)))))))
; 
; (define core-env
;   (build-env
;     gc-info gc-check gc booleans->integer list->integer integer->list reverse-bit-field 
;     rotate-bit-field ash arithmetic-shift copy-bit-field bit-field copy-bit logbit? bit-set? 
;     log2-binary-factors first-set-bit integer-length logcount bit-count logtest any-bits-set? 
;     bitwise-if bitwise-merge lognot bitwise-not logxor bitwise-xor logior bitwise-ior logand 
;     bitwise-and file-stat stack-frame-ref stack-frame-next-frame stack-frame-env stack-frame-pc 
;     stack-frame-length continuation->stack-frame stack-frame? continuation? %mysql:close 
;     %mysql:escape %mysql:free %mysql:row %mysql:fields %mysql:query %mysql:connect error trace-load 
;     load write-char newline display pretty-print write char-ready? eof-object? peek-char read-char 
;     read close-output-port close-input-port open-output-file open-input-file 
;     %set-current-output-port! %set-current-input-port! current-output-port current-input-port 
;     output-port? input-port? procedure? not boolean? string->keyword keyword->string keyword? 
;     gensym string->symbol symbol->string symbol? char-downcase char-upcase integer->char 
;     char->integer char-lower-case? char-upper-case? char-whitespace? char-numeric? char-alphabetic? 
;     char-ci>=? char-ci<=? char-ci>? char-ci<? char-ci=? char>=? char<=? char>? char<? char=? char? 
;     string-ci>=? string-ci<=? string-ci>? string-ci<? string-ci=? string>=? string<=? string>? 
;     string<? string=? string-fill! string-copy list->string string->list string-append substring 
;     string-set! string-ref string-length string make-string string? equal? eq? eqv? %record-set! 
;     %record-ref %record %make-record record? vector-set! vector-ref vector-length vector 
;     make-vector vector? string->number number->string / * - + > >= != = <= < max min expt atan sqrt 
;     acos asin tan cos sin log exp round truncate ceiling floor modulo remainder quotient abs even? 
;     odd? positive? negative? zero? inexact? exact? integer? rational? real? complex? number? assoc 
;     assv assq member memv memq list-ref list-tail reverse append length list list? null? set-cdr! 
;     set-car! cddddr cdddar cddadr cddaar cdaddr cdadar cdaadr cdaaar cadddr caddar cadadr cadaar 
;     caaddr caadar caaadr caaaar third cdddr cddar cdadr cdaar caddr cadar caadr caaar second cddr 
;     cdar cadr caar rest first cdr car cons pair? unquote-splicing unquote quasiquote
;     undefined void trace-eval apply %compile trace-compile))

; (define (ev exp)
;   (ev0 exp core-env))

(define-macro (cc exp)
  `(for-each
	(lambda (x) (write x) (newline))
	(cc:ev0 ',exp '())))
 

