; Virtual instructions:
;
;   (label LABEL)				-- declare LABEL
;   (template ARGC ARGV)        -- declare a closure taking ARGC arguments, and wanting "rest" args if ARGV is #t
;   (branch-false LABEL)        -- if !value then pc = LABEL
;   (branch-true LABEL)         -- if value then pc = LABEL
;   (branch LABEL)				-- pc = LABEL
;   (lit VALUE)					-- value = VALUE
;   (push)                      -- push VALUE to stack
;   (call-n ARG-COUNT)
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

; r5rs (wisp builtins)
(define cc:r5rs-builtin
  '(
     eqv? eq? equal?
     number? complex? real? rational? integer? exact? inexact?
     = < <= > >= zero? positive? negative? odd? even? max min
     + - * / abs quotient remainder modulo floor ceiling truncate round
     exp log sin cos tan asin acos atan sqrt expt
     exact->inexact inexact->exact
     number->string string->number
     not boolean?
     pair? list? null? cons car cdr set-car! set-cdr! list length
     append reverse list-tail list-ref memq memv member assq assv assoc
     symbol? symbol->string string->symbol
     char? char=? char<? char<=? char>? char>=? char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=? char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? char->integer integer->char char-upcase char-downcase
     string? make-string string string-length string-ref string-set! string=? string<? string<=? string>? string>=? string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=? substring string-append string->list list->string string-copy string-fill!
     vector? make-vector vector vector-length vector-ref vector-set! vector->list list->vector vector-fill!
     procedure? apply values call-with-values scheme-report-environment null-environment interaction-environment
     input-port? output-port? current-input-port current-output-port open-input-file open-output-file close-input-port close-output-port
     read-char peek-char eof-object? char-ready?
     newline write-char
     %call/cc

     %values->list  ;; not r5rs!
     ))

; r5rs (wisp library funcs) (TODO)
(define cc:r5rs-library
  '(
     gcd lcm
     map for-each force call-with-current-continuation dynamic-wind
     eval
     call-with-input-file call-with-output-file with-input-from-file with-output-to-file
     read
     load
     write display
     ))

; r5rs - unsupported (TODO?)
(define cc:r5rs-unsupported
  '(
     numerator denominator rationalize
     make-rectangular make-polar real-part imag-part magnitude angle
     port?
     transcript-on transcript-off
     ))

(define cc:srfi-60-builtin
  '(
     bitwise-and logand bitwise-ior logior bitwise-xor logxor bitwise-not lognot
     bitwise-merge bitwise-if any-bits-set? logtest bit-count logcount
     integer-length first-set-bit log2-binary-factors bit-set? logbit?
     copy-bit bit-field copy-bit-field arithmetic-shift ash rotate-bit-field
     reverse-bit-field integer->list list->integer booleans->integer
     ))

; (cc:build-env+mem <symbol> ...)
; Returns an environment/memory pair. The environment is an assoc list mapping
; symbols to memory locations. The memory is a vector of corresponding values.
; For example (cc:build-env+mem cons list append ...) returns:
;
;     ((env .
;         ((cons . 0)
;          (list . 1)
;          (append . 2)
;          ...)
;      (mem . #(
;         #<primitive:cons>
;         #<primitive:list>
;         #<primitive:append>
;         ...)))
;
(define (cc:build-env+mem syms)
  (let loop ((syms syms) (i 0) (env '()) (mem '()))
    (if (null? syms) 
      (list
         (cons 'env (reverse env))
         (cons 'mem (list->vector (reverse mem))))
      (loop
        (rest syms)
        (+ i 1)
        (cons (cons (car syms) i) env)
        (cons (eval (car syms) #f) mem)))))

(define *cc:global-env* '())

(define *cc:global-mem* #())

; Calculate the core environment
; TODO - we can instead interrogate interned-symbols...
; we should run (a version of) vm-define against all of
; the compiled-lambdas.
(let ((env+mem
        (cc:build-env+mem
          (append
            cc:r5rs-builtin
            cc:r5rs-library
            cc:srfi-60-builtin))))

  ; Extract the environment
  (set! *cc:global-env* (cdr (assq 'env env+mem)))

  ; Extract the initial memory state
  (set! *cc:global-mem* (cdr (assq 'mem env+mem)))
  )

(define *cc:global-index* (vector-length *cc:global-mem*))

(define *cc:global-pc* 0)

(define *cc:global-prog* #())

; (cc:make-linkage <code> ...)
; Returns a new linkage containing the supplied code, and no procs.
(define (cc:make-linkage . code)
  (list LINKAGE: code '()))

; (cc:make-linkage-with-procs <code-list> <proc-list>)
; Returns a new linkage containing the supplied code and procs.
(define (cc:make-linkage-with-procs code procs)
  (list LINKAGE: code procs))

; (cc:linkage-code <linkage>)
; Returns the code in a linkage.
(define cc:linkage-code second)

; (cc:linkage-procs <linkage>)
; Returns the procs in a linkage.
(define cc:linkage-procs third)

; (cc:join-linkages <linkage> ...)
; Returns a new linkage whose code consists of the code from each of the
; arguments, and procs from the procs of each of the arguments.
(define (cc:join-linkages . linkages)
  (cc:make-linkage-with-procs
    (append-map cc:linkage-code linkages)
    (append-map cc:linkage-procs linkages)))

; (cc:proc <expr> <env>)
; Returns a linkage that will evaluate <expr> into the value register, and then halt.
(define (cc:prog expr env)
  (cc:join-linkages
    (cc:evaluate expr env)
    (cc:make-linkage
      `(halt))))

; (cc:evaluate <expr> <env>)
; Returns a linkage that will evaluate <expr> into the value register.
(define (cc:evaluate expr env)
  (cond
    ((symbol? expr)
      (cc:env-get expr env))
    ((pair? expr)
      (let ((func (first expr))
             (args (rest expr)))
        (case func
          ((%lambda) (cc:lambda (first args) (rest args) env))
          ((if) (cc:if args env))
          ((and) (cc:and args env))
          ((or) (cc:or args env))
          ((set!) (cc:set! (first args) (second args) env))
          ((%define) (cc:define (first args) (second args) env))
          ((begin) (cc:begin args env))
          ((quote) (cc:literal (first args)))
          (else
            (cc:application func args env)))))
    (else
      (cc:literal expr))))

; (cc:env-extend <args> <env>)
; Returns a new environment containing a new copy of <args> prepended to <env>.
(define (cc:env-extend args env)
  (cons
    (let loop ((args args))
      (if (pair? args)
        (cons (first args) (loop (rest args)))
        (list args)))
    env))

; (cc:make-slot <frame:integer> <offset:integer> <name:string>)
; Returns a new slot reference for the value in the give <frame>
; at the given <offset>. The <name> solely acts as a comment.
(define (cc:make-slot frame offset name)
  (list frame offset name))

; (cc:slot-frame <slot>)
; Returns the frame index in <slot>.
(define cc:slot-frame first)

; (cc:slot-offset <slot>)
; Returns the slot index in <slot>.
(define cc:slot-offset second)

; (cc:slot-name <slot>)
; Returns the name comment in <slot>.
(define cc:slot-name third)

; (cc:env-lookup <arg:symbol> <env>)
; Returns a slot reference to <arg> if found in <env>, or #f otherwise.
(define (cc:env-lookup arg env)
  (let next-frame ((frame 0) (env env))
    (and
      (pair? env)
      (let next-slot ((offset 0) (frame-vars (first env)))
        (if (pair? frame-vars)
          (if (eq? arg (first frame-vars))
            (cc:make-slot frame offset arg)
            (next-slot (+ 1 offset) (rest frame-vars)))
          (next-frame (+ 1 frame) (rest env)))))))

; (cc:lambda <args:expr-list> <body:expr-list> <env>)
; Returns a linkage that will leave a closure in the value register. When
; invoked, the closure will evaluate <body> with <args> taken from the stack,
; and any free variables from <env>. The final result will be returned in the
; value register.
(define (cc:lambda args body env)
  (let ((label (cc:gen-label))
        (argc (length+ args))
        (argv (dotted-list? args)))
    (let ((body-linkage 
            (cc:join-linkages
              (cc:make-linkage
                `(template ,argc ,argv)
                `(label ,label))
              (cc:begin body (cc:env-extend args env))
              (cc:make-linkage
                `(return)))))
      (cc:make-linkage-with-procs
        `((make-closure ,label))
        (append 
          (cc:linkage-code body-linkage)
          (cc:linkage-procs body-linkage))))))

(define (cc:inlinable? func-sym env)
  (and
    (symbol? func-sym)
    (not (eq? func-sym 'apply)) ;; TODO - fix horrid special case
    (not (eq? func-sym '%call/cc)) ;; TODO - fix horrid special case
    (not (cc:env-lookup func-sym env))
    (memv func-sym cc:r5rs-builtin)
    (%func? (eval func-sym #f))))

(define (cc:inline-application func-sym args env)
  (let* ((func (eval func-sym #f))
          (argc (length args))
          (min-arity (%func-min-arity func))
          (max-arity (%func-max-arity func))
          (index (%func-index func)))
    (cond
      ((< argc min-arity)
        (error "too few arguments"))
      ((and max-arity (> argc max-arity))
        (error "too many arguments"))
      (else
        (cc:join-linkages
          ; write to frame instead of pushing?
          (cc:evaluate-args args env)
          (cc:literal index)
          (cc:make-linkage
            (case argc
              ((0) `(prim-0))
              ((1) `(prim-1))
              ((2) `(prim-2))
              ((3) `(prim-3))
              ((4) `(prim-4))
              (else
                `(prim-n ,argc)))))))))

; (cc:application <func:expr> <args:expr-list> <env>)
; Returns a linkage that will evaluate <args> leaving the results on the stack,
; then evaluate <func> to a procedure in the value register, and finally invoke
; the procedure with the evaluated arguments, leaving the result in the value
; register.
(define (cc:application func args env)
  (if (cc:inlinable? func env)
    (cc:inline-application func args env)
    (let ((argc (length args)))
      (cc:join-linkages
        (cc:evaluate-args args env)
        (cc:evaluate func env)
        (cc:make-linkage
          (case argc
            ((0) `(call-0))
            ((1) `(call-1))
            ((2) `(call-2))
            ((3) `(call-3))
            ((4) `(call-4))
            (else
              `(call-n ,argc))))))))

; (cc:evaluate-args <args:expr-list> <env>)
; Returns a linkage that will evaluate <args> leaving the results on the stack,
; and the value register in an indeterminate state.
(define (cc:evaluate-args args env)
  (let loop ((argi 0) (args args))
    (if (null? args)
      (cc:make-linkage)
      (cc:join-linkages
        (cc:evaluate (first args) env)
        (cc:make-linkage
          `(push))
        (loop (+ argi 1) (rest args))))))

; (cc:gen-label)
; Returns a new unique symbol which can be used as a label.
(define cc:gen-label
  (let ((label 0))
    (lambda ()
      (set! label (+ label 1))
      (string->symbol (string-append "L" (number->string label))))))


(define (cc:if args env)
  (case (length args)
    ((2) (cc:if2 (first args) (second args) env))
    ((3) (cc:if3 (first args) (second args) (third args) env))
    (else (cc:raise-error "if needs either 2 or 3 arguments"))))

; (cc:if2 <test:expr> <true-branch:expr> <env>)
; Returns a linkage that will evaluate <test>, and if true evaluate
; <true-branch> leaving the result in the value register. If false,
; the value register will contain the void value.
(define (cc:if2 test true-branch env)
  (let ((else-label (cc:gen-label))
        (endif-label (cc:gen-label)))
    (cc:join-linkages
      (cc:evaluate test env)
      (cc:make-linkage
        `(branch-false ,endif-label))
      (cc:evaluate true-branch env)
      (cc:make-linkage
        `(label ,endif-label)
        `(void)))))

; (cc:if3 <test:expr> <true-branch:expr> <false-branch:expr> <env>)
; Returns a linkage that will evaluate <test>, and if true evaluate
; <true-branch>, otherwise <false-branch>. The result of the evaluated
; branch will be left in the value register.
(define (cc:if3 test true-branch false-branch env)
  (let ((else-label (cc:gen-label))
        (endif-label (cc:gen-label)))
    (cc:join-linkages
      (cc:evaluate test env)
      (cc:make-linkage
        `(branch-false ,else-label))
      (cc:evaluate true-branch env)
      (cc:make-linkage
        `(branch ,endif-label)
        `(label ,else-label))
      (cc:evaluate false-branch env)
      (cc:make-linkage
        `(label ,endif-label)))))

(define (cc:logic exprs env empty-value branch-op)
  (case (length exprs)
    ((0) (cc:literal empty-value))
    ((1) (cc:evaluate (first exprs) env))
    (else
      (let ((end-label (cc:gen-label)))
        (let loop ((exprs exprs))
          (if (null? (rest exprs))
            (cc:join-linkages
              (cc:evaluate (first exprs) env)
              (cc:make-linkage
                `(label ,end-label)))
            (cc:join-linkages
              (cc:evaluate (first exprs) env)
              (cc:make-linkage `(,branch-op ,end-label))
              (loop (rest exprs)))))))))

; (cc:and <expr-list> <env>)
(define (cc:and exprs env)
  (cc:logic exprs env #t 'branch-false))

; (cc:or <expr-list> <env>)
(define (cc:or exprs env)
  (cc:logic exprs env #f 'branch-true))

; (cc:begin <expr-list> <env>)
; Returns a linkage that will evaluate each of the expressions in body.
; The final result will be left in the value register. If <body> is
; empty, value will be void.
  (define (cc:begin body env)
    (case (length body)
      ((0) (cc:make-linkage `(void)))
      ((1) (cc:evaluate (first body) env))
      (else
        (cc:join-linkages
          (cc:evaluate (first body) env)
          (cc:begin (rest body) env)))))

; (cc:literal <arg>)
; Returns a linkage that will set the value register to the literal <arg>.
(define (cc:literal arg)
  (cc:make-linkage
    `(lit ,arg)))

; (cc:define <var:symbol> <value:expr> <env>)
; Returns a linkage that will evaluate <value> and set the global value of <var>
; to the result.
(define (cc:define var value env)
  (cc:join-linkages
    (cc:evaluate value env)
    (cc:set-global! var)))

; (cc:set! <var:symbol> <value:expr> <env>)
; Returns a linkage that will evaluate <value>, and use the result to set the
; value of <var> in the environment if found in <env>, otherwise the global value.
(define (cc:set! var value env)
  (cc:join-linkages
    (cc:evaluate value env)
    (cond
      ((cc:env-lookup var env) => cc:set-slot!)
      (else (cc:set-global! var)))))

; (cc:set-slot! <slot>)
; Returns a linkage that will set the <slot> in the runtime environment to the
; contents of the value register.
(define (cc:set-slot! slot)
  (cc:make-linkage
    `(set-slot ,(cc:slot-frame slot) ,(cc:slot-offset slot) ,(cc:slot-name slot))
    `(void)))

; (cc:set-global! <var:symbol>)
; Returns a linkage that will set the global value of <var> to the contents of
; the value register.
(define (cc:set-global! var)
  (cc:make-linkage
    `(set-global ,@(cc:get-global-offset var))
    `(void)))

; TODO - is this correct?!
; (cc:get-global-offset <var:symbol>)
; Returns a reference to <var> in the global environment, creating a new entry
; first if necessary.
(define cc:get-global-offset
    (lambda (var)
      (cond
        ((assq var *cc:global-env*) => (lambda (v) (list (cdr v) (car v))))
        (else
          (let ((index *cc:global-index*))
            (set! *cc:global-env* (cons (cons var index) *cc:global-env*))
            ; horrendously inefficient!
            (set! *cc:global-mem* (list->vector (append (vector->list *cc:global-mem*) (list (undefined)))))
            (set! *cc:global-index* (+ *cc:global-index* 1))
            (list index var))))))

; (cc:env-get <var:symbol> <env>)
; Returns a linkage that will get the current value of <var> from the local
; environment if present, or otherwise from global memory, and leave the
; result in the value register.
(define (cc:env-get var env)
  (cond
    ((cc:env-lookup var env) => cc:get-slot)
    (else (cc:get-global var))))

; (cc:get-slot <slot>)
; Returns a linkage that will leave the contents of <slot> in the value register.
(define (cc:get-slot slot)
  (cc:make-linkage
    `(get-slot ,(cc:slot-frame slot) ,(cc:slot-offset slot) ,(cc:slot-name slot))))

; (cc:get-globlal <var:symbol>)
; Returns a linkage that will leave the value of <var> from global memory in the
; value register.
(define (cc:get-global var)
  (cc:make-linkage
    `(get-global ,@(cc:get-global-offset var))))

; (cc:raise-error <obj:string|symbol|number> ...)
; Raises an error with a message formed from the concatenation of the
; stringified form of its arguments.
(define (cc:raise-error . args)
  (error
    (apply string-append
           (map (lambda (x)
                  (cond ((string? x) x)
                        ((symbol? x) (symbol->string x))
                        ((number? x) (number->string x))
                        (else (error "bad args"))))
                args))))

; (cc:assemble <pc> <linkage>)
; Returns a two element list.
; The first element is the final pc.
; The second element is the assembled form of <linkage> as a vector of integer
; bytecodes and literal values.
(define (cc:assemble pc linkage)
  (let* ((code (append (cc:linkage-code linkage)
                       (cc:linkage-procs linkage)))
         (pc+labels (cc:compute-labels code pc '())))
    (list (first pc+labels) (cc:assembly->program code (second pc+labels)))))

; (cc:compute-labels <code:instruction-list> <pc:integer> <labels:assoc-list>)
; Returns an augmented list of <labels. Iterates <code> maintaining the program
; counter value (initalised to <pc>), looking for (label <label:symbol>) forms.
; The <labels> assoc-list is extended with new entries associating each label
; with its corresponding program counter value.
(define (cc:compute-labels code pc labels)
  (if (null? code) (list pc labels)
    (let* ((instruction (first code))
           (opcode (first instruction)))
      (cc:compute-labels
        (rest code)
        (+ pc (cc:opcode->length opcode))
        (if (eq? opcode 'label)
          (cons (cons (second instruction) pc) labels)
          labels)))))

; List of opcodes support by the vm - see src/vm.c
(define cc:vm-branch-false     0)
(define cc:vm-branch-true      1)
(define cc:vm-branch           2)
(define cc:vm-lit              3)
(define cc:vm-push             4)
(define cc:vm-set-global       5)
(define cc:vm-get-global       6)
(define cc:vm-set-slot         7)
(define cc:vm-get-slot         8)
(define cc:vm-make-closure     9)
(define cc:vm-return          10)
(define cc:vm-void            11)
(define cc:vm-halt            12)
(define cc:vm-call-0          13)
(define cc:vm-call-1          14)
(define cc:vm-call-2          15)
(define cc:vm-call-3          16)
(define cc:vm-call-4          17)
(define cc:vm-call-n          18)
(define cc:vm-prim-0          19)
(define cc:vm-prim-1          20)
(define cc:vm-prim-2          21)
(define cc:vm-prim-3          22)
(define cc:vm-prim-4          23)
(define cc:vm-prim-n          24)

; An assoc-list mapping each instruction name to (<opcode> <length>).
; If <opcode> is #f it indicates that there is no corresponding opcode.
(define cc:opcode-metadata
  `((branch-false    ,cc:vm-branch-false    2)
    (branch-true     ,cc:vm-branch-true     2)
    (branch          ,cc:vm-branch          2)
    (lit             ,cc:vm-lit             2)
    (push            ,cc:vm-push            1)
    (set-global      ,cc:vm-set-global      2)
    (get-global      ,cc:vm-get-global      2)
    (set-slot        ,cc:vm-set-slot        2)
    (get-slot        ,cc:vm-get-slot        2)
    (make-closure    ,cc:vm-make-closure    2)
    (return          ,cc:vm-return          1)
    (void            ,cc:vm-void            1)
    (halt            ,cc:vm-halt            1)
    (call-0          ,cc:vm-call-0          1)
    (call-1          ,cc:vm-call-1          1)
    (call-2          ,cc:vm-call-2          1)
    (call-3          ,cc:vm-call-3          1)
    (call-4          ,cc:vm-call-4          1)
    (call-n          ,cc:vm-call-n          2)
    (prim-0          ,cc:vm-prim-0          1)
    (prim-1          ,cc:vm-prim-1          1)
    (prim-2          ,cc:vm-prim-2          1)
    (prim-3          ,cc:vm-prim-3          1)
    (prim-4          ,cc:vm-prim-4          1)
    (prim-n          ,cc:vm-prim-n          2)
    (label           #f                     0)
    (template        #f                     2)))

; (cc:opcode->bytecode <opcode:symbol>)
; Returns the integer bytecode corresponding to <opcode>)
(define (cc:opcode->bytecode opcode)
  (cond ((assq opcode cc:opcode-metadata) => second)
        (else (cc:raise-error "unknown opcode " opcode))))

; (cc:opcode->length <opcode:symbol>)
; Returns the length of the encoded <opcode> including arguments.
(define (cc:opcode->length opcode)
  (cond ((assq opcode cc:opcode-metadata) => third)
        (else (cc:raise-error "unknown opcode " opcode))))

; (cc:assembly->program <code:instruction-list> <labels:assoc-list>)
; Returns a vector containing the bytecode corresponding to the given
; <code> and <labels>.
(define (cc:assembly->program code labels)
  (list->vector
    (let loop ((code code))
      (if (null? code) '()
        (let* ((instruction (first code))
               (opcode (first instruction))
               (bytecode (cc:opcode->bytecode opcode)))
          (case opcode
            ((label)
             (loop (rest code)))

            ((template)
             (cons (second instruction)
                   (cons (third instruction)
                         (loop (rest code)))))

            ((branch branch-false branch-true make-closure)
             (cons bytecode
                   (cons (cdr (assq (second instruction) labels))
                         (loop (rest code)))))

            ((lit call-n set-global get-global)
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

; (cc:simulate <prog:bytecode-vector> <mem:obj-vector>)
; Returns the content of the value register after simulating <prog>.
(define (cc:simulate pc prog mem)
  (define value #f)
  (define stack '())
  (define env '())
  (define tpl #())

  (define (prog-ref i) (vector-ref prog i))

  (define (prog-fetch)
    (let ((v (prog-ref pc)))
      (display " ") (display v)
      (set! pc (+ pc 1))
      v))

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

  ;; call function in value, with ARG-COUNT args from stack
  ;; if value contains a builtin, copy the args into a frame, call it, and leave the result in value
  ;; if value contains a closure, copy the args into a frame
  (define (call argc)
    (let ((frame (stack-pop-frame argc)))
      (if (and (pair? value)
            (eq? (car value) closure:))
        (begin
          (stack-push pc)
          (stack-push env)
          (let* ((closure-pc (second value))
                  (closure-env (third value))
                  (want-argc (prog-ref (- closure-pc 2)))
                  (want-argv (prog-ref (- closure-pc 1))))
            (display " want-argc:") (display want-argc)
            (display " want-argv:") (display want-argv)
            (newline)
            (set! pc closure-pc)
            (set! env (cons
                        (cond
                          (want-argv
                            (if (< argc want-argc)
                              (return-with-value
                                `(exception: "wanted at least " ,want-argc " args, but received " ,argc))
                              (list->vector (append (take frame want-argc) (list (drop frame want-argc))))))
                          (else
                            (if (= argc want-argc)
                              (list->vector frame)
                              (return-with-value
                                `(exception: "wanted " ,want-argc " args, but received " ,argc)))))
                        closure-env))))
        (set! value (apply value frame)))))

  (call/cc
    (lambda (return-with-value)
      (let loop ()
        (display "[" ) (display pc) (display "]")
        (let ((instruction (prog-fetch)))
          (let* ((name (symbol->string (first (list-ref cc:opcode-metadata instruction))))
                  (name-len (string-length name)))
            (display " ")
            (display name)
            (let lp ((pos name-len))
              (if (< pos 12) (begin (display " ") (lp (+ pos 1))))))
          (cond
            ((eq? instruction cc:vm-branch-false)     (let ((label (prog-fetch))) (if (not value) (set! pc label))))
            ((eq? instruction cc:vm-branch-true)      (let ((label (prog-fetch))) (if value (set! pc label))))
            ((eq? instruction cc:vm-branch)           (set! pc (prog-fetch)))
            ((eq? instruction cc:vm-lit)              (set! value (prog-fetch)))
            ((eq? instruction cc:vm-push)             (stack-push value))

            ;; reserve space in global symbol table if needed
            ; ((declare-global)
            ;  ...)

            ((eq? instruction cc:vm-set-global)       (vector-set! mem (prog-fetch) value))
            ((eq? instruction cc:vm-get-global)       (set! value (vector-ref mem (prog-fetch))))
            ((eq? instruction cc:vm-set-slot)         (let* ((v (prog-fetch)) (frame (quotient v 65536)) (slot (modulo v 65536))) (set-env frame slot value)))
            ((eq? instruction cc:vm-get-slot)         (let* ((v (prog-fetch)) (frame (quotient v 65536)) (slot (modulo v 65536))) (set! value (get-env frame slot))))
            ((eq? instruction cc:vm-make-closure)     (set! value (list closure: (prog-fetch) env)))
            ((eq? instruction cc:vm-return)           (set! env (stack-pop)) (set! pc (stack-pop)))
            ((eq? instruction cc:vm-void)             (set! value (void)))
            ((eq? instruction cc:vm-halt)             (return-with-value `(result: ,value)))
            ((eq? instruction cc:vm-call-0)           (call 0))
            ((eq? instruction cc:vm-call-1)           (call 1))
            ((eq? instruction cc:vm-call-2)           (call 2))
            ((eq? instruction cc:vm-call-3)           (call 3))
            ((eq? instruction cc:vm-call-4)           (call 4))
            ((eq? instruction cc:vm-call-n)           (call (prog-fetch)))
            ((eq? instruction cc:vm-prim-0)           (prim 0))
            ((eq? instruction cc:vm-prim-1)           (prim 1))
            ((eq? instruction cc:vm-prim-2)           (prim 2))
            ((eq? instruction cc:vm-prim-3)           (prim 3))
            ((eq? instruction cc:vm-prim-4)           (prim 4))
            (else
              (return-with-value `(exception: "unknown-instruction" ,instruction))))

          (newline)
          (loop))))))

(define (vector-append . vecs)
  (list->vector (append-map vector->list vecs)))

; (vm-run <expr>)
; Compiles <expr>, executes it in the virtual machine, and returns the result.
(define-macro (vm-run expr)
  `(let*
     ((linkage (cc:prog ',(core:macro-expand expr) '()))
       (current-pc *cc:global-pc*)
       (pc+prog (cc:assemble current-pc linkage))
       (combined-prog (vector-append *cc:global-prog* (second pc+prog))))
     (set! *cc:global-prog* combined-prog)
     (set! *cc:global-pc* (first pc+prog))
     (%vm-run current-pc *cc:global-prog* *cc:global-mem*)))

; Debugging

; (cc:display-list <list>)
; Displays <list> as a newline-separated, numbered list.
(define (cc:display-list lis)
  (let loop ((n 0) (lis lis))
    (cond ((not (null? lis))
           (display n) (display "\t") (display (first lis)) (newline)
           (loop (+ n 1) (rest lis))))))

; (assemble <expr>)
; Compiles <expr> and displays the assembled output symbolically.
(define-macro (asm expr)
  `(let ((linkage (cc:prog ',(%macro-expand expr) '())))
     (cc:display-list (append (cc:linkage-code linkage) (cc:linkage-procs linkage)))))

; (disass <expr>)
; Compiles <expr> and displays annotated bytecode.
(define-macro (disass expr)
  `(let* ((linkage (cc:prog ',(%macro-expand expr) '()))
           (pc+prog (cc:assemble 0 linkage))
           (prog (second pc+prog))
           (prog-len (vector-length prog)))
     (let loop ((pc 0))
        (if (< pc prog-len)
          (let* ((opcode (vector-ref prog pc))
                 (meta (list-ref cc:opcode-metadata opcode))
                 (instr (first meta))
                 (instr-len (third meta)))
            (display pc) (display " : ")
            (display opcode) (display " ")
            (display instr)
            (let loop ((j 1))
              (if (< j instr-len)
                (begin
                  (display " ")
                  (display (vector-ref prog (+ pc j)))
                  (loop (+ j 1)))))
            (newline)
            (loop (+ pc instr-len)))))))

; (cc <expr>)
; Compiles <expr> and displays raw bytecode.
(define-macro (cc expr)
  `(let ((linkage (cc:prog ',(%macro-expand expr) '())))
     (cc:display-list (vector->list (cc:assemble pc linkage)))))

; (simulate <expr>)
; Compiles <expr> and runs it in the simulator.
(define-macro (simulate expr)
  `(let ((linkage (cc:prog ',(%macro-expand expr) '())))
     (let ((pc+prog (cc:assemble 0 linkage)))
       (cc:simulate 0 (second pc+prog) *cc:global-mem*))))

