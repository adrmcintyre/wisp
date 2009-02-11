; Virtual instructions so far:
;   (label LABEL)				-- declare LABEL
;   (template ARGC ARGV)
;   (branch-if-false LABEL)     -- if !value then pc = LABEL
;   (branch LABEL)				-- pc = LABEL
;   (lit VALUE)					-- value = VALUE
;   (push)                      -- push VALUE
;   (call ARG-COUNT)
;       -- call function in value with ARG-COUNT args from the stack
;       -- if value contains a builtin,
;       --   pop the args into a frame,
;       --   push env, pc
;       --   call the proc and leave the result in value
;       -- if value contains a closure, copy the args into a frame
;
;   (declare-global SYM)		-- reserve space in global symbol table if needed
;   (set-global SYM)			-- *locate_global(SYM) = value
;   (get-global SYM)			-- value = *locate_global(SYM)
;	(set-slot FRAME SLOT)		-- *locate_slot(FRAME,SLOT) = value
;   (get-slot FRAME SLOT)		-- value = *locate_slot(FRAME,SLOT)
;   (make-closure LABEL)		-- value = make_closure(LABEL,env)
;   (return)					-- pop pc, env
;   (void)						-- value = void
;   (halt)						-- terminate machine, returning value as final result
;
; Registers:
;   value
;   env
;   pc
;   sp

(require "srfi/1")

(define-macro (build-env . syms)
  (let loop ((syms syms) (i 0) (env '()) (mem '()))
    (if (null? syms) 
      `(list
         (cons 'env (list   . ,(reverse env)))
         (cons 'mem (vector . ,(reverse mem))))
      (loop
        (rest syms)
        (+ i 1)
        (cons `(cons ',(car syms) ,i) env)
        (cons (car syms) mem)))))

(define core-env
  (build-env
    gc-info gc-check gc booleans->integer list->integer integer->list reverse-bit-field 
    rotate-bit-field ash arithmetic-shift copy-bit-field bit-field copy-bit logbit? bit-set? 
    log2-binary-factors first-set-bit integer-length logcount bit-count logtest any-bits-set? 
    bitwise-if bitwise-merge lognot bitwise-not logxor bitwise-xor logior bitwise-ior logand 
    bitwise-and file-stat stack-frame-ref stack-frame-next-frame stack-frame-env stack-frame-pc 
    stack-frame-length continuation->stack-frame stack-frame? continuation? 
    error trace-load 
    load write-char newline display pretty-print write char-ready? eof-object? peek-char read-char 
    read close-output-port close-input-port open-output-file open-input-file 
    %set-current-output-port! %set-current-input-port! current-output-port current-input-port 
    output-port? input-port? procedure? not boolean? string->keyword keyword->string keyword? 
    gensym string->symbol symbol->string symbol? char-downcase char-upcase integer->char 
    char->integer char-lower-case? char-upper-case? char-whitespace? char-numeric? char-alphabetic? 
    char-ci>=? char-ci<=? char-ci>? char-ci<? char-ci=? char>=? char<=? char>? char<? char=? char? 
    string-ci>=? string-ci<=? string-ci>? string-ci<? string-ci=? string>=? string<=? string>? 
    string<? string=? string-fill! string-copy list->string string->list string-append substring 
    string-set! string-ref string-length string make-string string? equal? eq? eqv? %record-set! 
    %record-ref %record %make-record record? vector-set! vector-ref vector-length vector 
    make-vector vector? string->number number->string / * - + > >= != = <= < max min expt atan sqrt 
    acos asin tan cos sin log exp round truncate ceiling floor modulo remainder quotient abs even? 
    odd? positive? negative? zero? inexact? exact? integer? rational? real? complex? number? assoc 
    assv assq member memv memq list-ref list-tail reverse append length list list? null? set-cdr! 
    set-car! cddddr cdddar cddadr cddaar cdaddr cdadar cdaadr cdaaar cadddr caddar cadadr cadaar 
    caaddr caadar caaadr caaaar third cdddr cddar cdadr cdaar caddr cadar caadr caaar second cddr 
    cdar cadr caar rest first cdr car cons pair? unquote-splicing unquote quasiquote
    undefined void apply %compile trace-compile))

; env is a list of ((symbol ...) (symbol ...) ...)
(define *global-env* (cdr (assoc 'env core-env)))
(define *global-mem* (cdr (assoc 'mem core-env)))

(define (make-linkage . code)
  (list LINKAGE: code '()))

(define (make-linkage-with-procs code procs)
  (list LINKAGE: code procs))

(define linkage-code second)
(define linkage-procs third)

(define (join-linkages . linkages)
  (make-linkage-with-procs
    (append-map linkage-code linkages)
    (append-map linkage-procs linkages)))

(define (cc:prog exp env)
  (join-linkages
    (cc:ev exp env)
    (make-linkage
      `(halt))))

; leaves result in value
; exp env -> linkage
(define (cc:ev exp env)
  (cond
    ((symbol? exp)
     (cc:env-get exp env))

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
    (else
      (cc:literal exp))))

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

; leaves result in value
(define (cc:lambda args body env)
  (let ((label (gen-label))
        (argc (length+ args))
        (argv (dotted-list? args)))
    (let ((body-linkage 
            (join-linkages
              (make-linkage
                `(template ,argc ,argv)
                `(label ,label))
              (cc:begin body (env-extend args env))
              (make-linkage
                `(return)))))
      (make-linkage-with-procs
        `((make-closure ,label))
        (append 
          (linkage-code body-linkage)
          (linkage-procs body-linkage))))))

; leaves result in value
(define (cc:application func args env)
  (join-linkages
    (cc:ev-args args env)
    (cc:ev func env)
    (make-linkage
      `(call ,(length args)))))

; pushes all results to stack
; trashes value
(define (cc:ev-args args env)
  (if (null? args) (make-linkage)
    (join-linkages
      (cc:ev (first args) env)
      (make-linkage
        `(push))
      (cc:ev-args (rest args) env))))

(define gen-label
  (let ((label 0))
    (lambda ()
      (set! label (+ label 1))
      (string->symbol (string-append "L" (number->string label))))))


; leaves void in value
(define (cc:if2 test true-branch env)
  (let ((else-label (gen-label))
        (endif-label (gen-label)))
    (join-linkages
      (cc:ev test env)
      (make-linkage
        `(branch-if-false ,endif-label))
      (cc:ev true-branch env)
      (make-linkage
        `(label ,endif-label)
        `(void)))))

; leaves result in value
(define (cc:if3 test true-branch false-branch env)
  (let ((else-label (gen-label))
        (endif-label (gen-label)))
    (join-linkages
      (cc:ev test env)
      (make-linkage
        `(branch-if-false ,else-label))
      (cc:ev true-branch env)
      (make-linkage
        `(branch ,endif-label)
        `(label ,else-label))
      (cc:ev false-branch env)
      (make-linkage
        `(label ,endif-label)))))

; leaves result in value
(define (cc:begin body env)
  (case (length body)
    ((0) (make-linkage `(void)))
    ((1) (cc:ev (first body) env))
    (else
      (join-linkages
		(cc:ev (first body) env)
		(cc:begin (rest body) env)))))

; leaves result in value
(define (cc:literal arg)
  (make-linkage
    `(lit ,arg)))

; leaves void in value
(define (cc:define var value env)
  (join-linkages
    (cc:ev value env)
    (cc:set-global! var)))

; leaves void in value
(define (cc:set! var value env)
  (join-linkages
    (cc:ev value env)
    (cond
      ((env-lookup var env) => cc:set-slot!)
      (else (cc:set-global! var)))))

; leaves void in value
(define (cc:set-slot! slot)
  (make-linkage
    `(set-slot ,(slot-frame slot) ,(slot-offset slot) ,(slot-name slot))
    `(void)))

; leaves void in value
(define (cc:set-global! var)
  (make-linkage
    `(set-global ,@(get-global-offset var))
    `(void)))

(define get-global-offset
  (let ((index 0))
    (lambda (var)
      (cond
        ((assoc var *global-env*) => (lambda (v) (list (cdr v) (car v))))
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
  (make-linkage
    `(get-slot ,(slot-frame slot) ,(slot-offset slot) ,(slot-name slot))))

; leaves result in value
(define (cc:get-global var)
  (make-linkage
    `(get-global ,@(get-global-offset var))))

(define (raise-error . args)
  (error
    (apply string-append
           (map (lambda (x)
                  (cond ((string? x) x)
                        ((symbol? x) (symbol->string x))
                        ((number? x) (number->string x))
                        (else (error "bad args"))))
                args))))


(define (assemble linkage)
  (let* ((code (append (linkage-code linkage)
                       (linkage-procs linkage)))
         (labels (compute-labels code 0 '())))
    (assembly->program code labels)))

(define (compute-labels code pc labels)
  (if (null? code) labels
    (let* ((instruction (first code))
           (opcode (first instruction)))
      (compute-labels
        (rest code)
        (+ pc (opcode->length opcode)) 
        (if (eq? opcode 'label)
          (cons (cons (second instruction) pc) labels)
          labels)))))

(define opcode-metadata
  '((branch-if-false  0 2)   
    (branch           1 2)   
    (lit              2 2)   
    (push             3 1)   
    (set-global       4 2)   
    (get-global       5 2)   
    (set-slot         6 2)   
    (get-slot         7 2)   
    (make-closure     8 2)   
    (return           9 1)   
    (void            10 1)   
    (halt            11 1)   
    (call            12 2)
    (label           #f 0)
    (template        #f 2)))

(define (opcode->bytecode opcode)
  (cond ((assoc opcode opcode-metadata) => second)
        (else (raise-error "unknown opcode " opcode))))

(define (opcode->length opcode)
  (cond ((assoc opcode opcode-metadata) => third)
        (else (raise-error "unknown opcode " opcode))))

(define (assembly->program code labels)
  (list->vector
    (let loop ((code code))
      (if (null? code) '()
        (let* ((instruction (first code))
               (opcode (first instruction))
               (bytecode (opcode->bytecode opcode)))
          (case opcode
            ((label)
             (loop (rest code)))

            ((template)
             (cons (second instruction)
                   (cons (third instruction)
                         (loop (rest code)))))

            ((branch branch-if-false make-closure)
             (cons bytecode
                   (cons (cdr (assoc (second instruction) labels))
                         (loop (rest code)))))

            ((lit call set-global get-global)
             (cons bytecode
                   (cons (second instruction)
                         (loop (rest code)))))

            ((set-slot get-slot)
             (cons bytecode
                   (cons (+ (* (second instruction) 65536)
                            (third instruction))
                     (loop (rest code)))))

            (else
              (cons bytecode (loop (rest code))))))))))

(define (run-program program)
  (define value #f)
  (define pc 0)
  (define stack '())
  (define env '())
  (define tpl #())

  (define (stack-push v)
    (set! stack (cons v stack)))

  (define (stack-pop)
    (let ((tos (first stack)))
      (set! stack (rest stack))
      tos))

  (define (stack-pop-frame n)
    (let loop ((n n) (result '()))
      (if (zero? n) result
        (loop
          (- n 1)
          (cons (stack-pop) result)))))

  (define (get-env depth offset)
    (let loop ((env env) (depth depth))
      (if (zero? depth)
        (vector-ref (first env) offset)
        (loop (rest env) (- depth 1)))))

  (define (set-env depth offset v)
    (let loop ((env env) (depth depth))
      (if (zero? depth)
        (vector-set! (first env) offset v)
        (loop (rest env) (- depth 1)))))

  (call/cc
    (lambda (return-with-value)
      (let loop ()
        (let ((instruction (vector-ref program pc)))
          (display pc) (display ": ") (display instruction) (newline)
          (set! pc (+ pc 1))
          (case (first instruction)

            ((branch-if-false)  (if (not value) (set! pc (second instruction))))
            ((branch)           (set! pc (second instruction)))
            ((lit)              (set! value (second instruction)))
            ((push)             (stack-push value))

            ;; reserve space in global symbol table if needed
            ; ((declare-global)
            ;  ...)

            ((set-global)       (vector-set! *global-mem* (second instruction) value))
            ((get-global)       (set! value (vector-ref *global-mem* (second instruction))))
            ((set-slot)         (set-env (second instruction) (third instruction) value))
            ((get-slot)         (set! value (get-env (second instruction) (third instruction))))
            ((make-closure)     (set! value (list closure: (second instruction) env)))
            ((return)           (set! env (stack-pop)) (set! pc  (stack-pop)))
            ((void)             (set! value (void)))
            ((halt)             (return-with-value `(result: ,value)))

            ;; call function in value, with ARG-COUNT args from stack
            ;; if value contains a builtin, copy the args into a frame, call it, and leave the result in value
            ;; if value contains a closure, copy the args into a frame
            ((call)
             (let* ((tag (first value))
                    (argc (second instruction))
                    (frame (stack-pop-frame argc)))

               (case tag
                 ((builtin:)
                  (set! value (apply (second value) frame)))

                 ((closure:)
                  (stack-push pc)
                  (stack-push env)
                  (let* ((closure-pc (second value))
                         (closure-env (third value))
                         (tpl (vector-ref program (- closure-pc 1)))
                         (want-argc (second tpl))
                         (want-argv (third tpl)))
                    (set! pc closure-pc)
                    (set! env (cons
                                (cond
                                  (want-argv
                                    (if (< argc want-argc)
                                      (return-with-value
                                        `(exception: "wanted at least " ,want-argc " args, but received " ,argc))
                                      (list->vector (append (take frame want-argc) (list (drop frame want-argc))))))
                                  (else
                                    (if (!= argc want-argc)
                                      (return-with-value
                                        `(exception: "wanted " ,want-argc " args, but received " ,argc))
                                      (list->vector frame))))
                                closure-env))))
                 (else
                   (return-with-value `(exception: "unknown-tag" ,tag))))))

            (else
              (return-with-value `(exception: "unknown-instruction" ,instruction))))

          (loop))))))


(define (display-list lis)
  (let loop ((n 0) (lis lis))
    (cond ((not (null? lis))
           (display n) (display "\t") (display (first lis)) (newline)
           (loop (+ n 1) (rest lis))))))

(define-macro (cc exp)
  `(let ((linkage (cc:prog ',exp '())))
     (display-list (vector->list (assemble linkage)))))
 
(define-macro (asm exp)
  `(let ((linkage (cc:prog ',exp '())))
     (display-list (append (linkage-code linkage) (linkage-procs linkage)))))

(define-macro (run exp)
  `(let ((linkage (cc:prog ',exp '())))
     (run-program (assemble linkage))))
 
(define-macro (vm exp)
  `(let ((linkage (cc:prog ',exp '())))
     (vm-run (assemble linkage) *global-mem*)))

