; instructions:
;   assign
;   branch
;   goto
;   'label
;   perform
;   restore
;   save
;   test

; registers:
;   argl
;   continue
;   env
;   proc
;   val

; operations:
;   cons CAR CDR
;   list ELT
;   compiled-procedure-env PROC
;   extend-environment FORMAL-PARAMS ARGS ENV
;   lookup-variable-value SYM ENV
;   apply-primitive-procedure PROC ARGS
;   compiled-procedure-entry PROC
;   lookup-variable-value N ENV
;   make-compiled-procedure LABEL ENV
; ...

; operands:
;   (const K)
;   (reg R)
;   (label L)

(define (begin-actions exp)
  (rest exp))

; leaves result in value
(define (compile exp target linkage)
  (cond
    ((cc:literal? exp)
     (compile-self-evaluating exp target linkage))
    ((cc:quote? exp) 
     (compile-quoted exp target linkage))
    ((cc:symbol? exp) 
     (compile-variable exp target linkage))
    ((cc:set? exp)
     (compile-assignment exp target linkage))
    ((cc:define? exp)
     (compile-definition exp target linkage))
    ((cc:if? exp) 
     (compile-if exp target linkage))
    ((cc:lambda? exp)
     (compile-lambda exp target linkage))
    ((cc:begin? exp) 
     (compile-sequence (begin-actions exp)
                       target
                       linkage))
    ((cc:application? exp)
     (compile-application exp target linkage))
    (else
      (raise-error "uncompilable expression: " exp))))

(define (cc:literal? exp)
  (not
    (or
      (pair? exp)
      (symbol? exp))))

(define (cc:quote? exp)
  (and
    (pair? exp)
    (eq? (first exp) 'quote)))

(define (cc:symbol? exp)
  (symbol? exp))

(define (cc:if? exp)
  (case (length exp)
    ((4) (eq? (first exp) 'if))
    (else #f)))

(define (cc:set? exp)
  (and (= (length exp) 3)
       (eq? (first exp) 'set!)))

(define (cc:define? exp)
  (and (= (length exp) 3)
       (eq? (first exp) 'define)))

(define (cc:begin? exp)
  (and (>= (length exp) 1)
       (eq? (first exp) 'begin)))

(define (cc:lambda? exp)
  (and (>= (length exp) 3)
       (eq? (first exp) 'lambda)))

(define (cc:application? exp)
  (>= (length exp) 1))
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))




(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (case linkage
    ((return)
     (make-instruction-sequence '(continue) '()
       '((goto (reg continue)))))
    ((next)
     (empty-instruction-sequence))
    (else
      (make-instruction-sequence '() '()
        `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
    instruction-sequence
    (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '() (list target)
      `((assign ,target (const ,exp))))))

(define (text-of-quotation exp)
  (second exp))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '() (list target)
      `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '(env) (list target)
      `((assign ,target
                (op lookup-variable-value)
                (const ,exp)
                (reg env))))))

(define (assignment-variable exp)
  (second exp))
(define (assignment-value exp)
  (third exp))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
      (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          `((perform (op set-variable-value!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))

(define (definition-variable exp)
  (second exp))
(define (definition-value exp)
  (third exp))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
      (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          `((perform (op define-variable!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

(define (if-predicate exp)
  (second exp))
(define (if-consequent exp)
  (third exp))
(define (if-alternative exp)
  (cadddr exp))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
              (compile
                (if-consequent exp) target consequent-linkage))
            (a-code
              (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
          p-code
          (append-instruction-sequences
            (make-instruction-sequence '(val) '()
              `((test (op false?) (reg val))
                (branch (label ,f-branch))))
            (parallel-instruction-sequences
              (append-instruction-sequences t-branch c-code)
              (append-instruction-sequences f-branch a-code))
            after-if))))))

(define (last-exp? seq)
  (and (pair? seq)
       (null? (rest seq))))
(define (first-exp seq)
  (first seq))
(define (rest-exps seq)
  (rest seq))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
    (compile (first-exp seq) target linkage)
    (preserving '(env continue)
      (compile (first-exp seq) target 'next)
      (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
            (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
            (make-instruction-sequence '(env) (list target)
              `((assign ,target
                        (op make-compiled-procedure)
                        (label ,proc-entry)
                        (reg env)))))
          (compile-lambda-body exp proc-entry))
        after-lambda))))

(define (lambda-parameters exp)
  (second exp))
(define (lambda-body exp)
  (cddr exp))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
        `(,proc-entry
           (assign env (op compiled-procedure-env) (reg proc))
           (assign env
                   (op extend-environment)
                   (const ,formals)
                   (reg argl)
                   (reg env))))
      (compile-sequence (lambda-body exp) 'val 'return))))

(define (operator application)
  (first application))
(define (operands application)
  (rest application))

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
          (map (lambda (operand) (compile operand 'val 'next))
               (operands exp))))
    (preserving '(env continue)
      proc-code
      (preserving '(proc continue)
        (construct-arglist operand-codes)
        (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
        '((assign argl (const ()))))
      (let ((code-to-get-last-arg
              (append-instruction-sequences
                (first operand-codes)
                (make-instruction-sequence '(val) '(argl)
                  '((assign argl (op list) (reg val)))))))
        (if (null? (rest operand-codes))
          code-to-get-last-arg
          (preserving '(env)
            code-to-get-last-arg
            (code-to-get-rest-args
              (rest operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
            (first operand-codes)
            (make-instruction-sequence '(val argl) '(argl)
              '((assign argl
                 (op cons) (reg val) (reg argl)))))))
    (if (null? (rest operand-codes))
      code-for-next-arg
      (preserving '(env)
        code-for-next-arg
        (code-to-get-rest-args (rest operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
            (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
          `((test (op primitive-procedure?) (reg proc))
            (branch (label ,primitive-branch))))
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-branch
            (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage linkage
              (make-instruction-sequence '(proc argl)
                                         (list target)
                `((assign ,target
                          (op apply-primitive-procedure)
                          (reg proc)
                          (reg argl)))))))
        after-call))))

(define all-regs '(env proc val argl continue))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
           '((assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val) (eq? linkage 'return)))
         (raise-error "return linkage, target not val -- COMPILE"
                      target))))

(define (registers-needed s)
  (if (symbol? s) '() (first s)))
(define (registers-modified s)
  (if (symbol? s) '() (second s)))
(define (statements s)
  (if (symbol? s) (list s) (third s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (list-difference (registers-needed seq2)
                                   (registers-modified seq1)))
      (list-union (registers-modified seq1)
                  (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
      (empty-instruction-sequence)
      (append-2-sequences (first seqs)
                          (append-seq-list (rest seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (first s1) s2) (list-union (rest s1) s2))
        (else (cons (first s1) (list-union (rest s1) s2)))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (first s1) s2) (list-difference (rest s1) s2))
        (else (cons (first s1)
                    (list-difference (rest s1) s2)))))
                              
(define (preserving regs seq1 seq2)
  (if (null? regs)
    (append-instruction-sequences seq1 seq2)
    (let ((first-reg (first regs)))
      (if (and (needs-register? seq2 first-reg)
               (modifies-register? seq1 first-reg))
        (preserving (rest regs)
          (make-instruction-sequence
            (list-union (list first-reg)
                        (registers-needed seq1))
            (list-difference (registers-modified seq1)
                             (list first-reg))
            (append `((save ,first-reg))
                    (statements seq1)
                    `((restore ,first-reg))))
          seq2)
        (preserving (rest regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2))
    (list-union (registers-modified seq1)
                 (registers-modified seq2))
    (append (statements seq1) (statements seq2))))


(define-macro (cc exp)
  `(for-each
	(lambda (x) (write x) (newline))
	(third (compile ',exp 'val 'next))))
 

(define prog '(define fact 
                (lambda (n)
                  (if (eq? n 0) 1
                    (* n (fact (- n 1)))))))

(define $ (third (compile prog 'val 'next)))
(define $$ (map (lambda(x) (if (pair? x) (first x) 'label)) $))

(define (sort <? lis)
  (cond ((null? lis) lis)
        ((null? (rest lis)) lis)
        (else
          (let ((sub-lists (odds+evens lis)))
            (merge <? (sort <? (first sub-lists))
                      (sort <? (second sub-lists)))))))

(define (odds+evens lis)
  (let loop ((lis lis)
             (odds '())
             (evens '()))
    (cond
      ((null? lis) (list odds evens))
      ((null? (rest lis)) (list (cons (first lis) odds) evens))
      (else
        (loop (cddr lis)
              (cons (first lis) odds)
              (cons (second lis) evens))))))

(define (merge <? lis1 lis2)
  (cond ((null? lis1) lis2)
        ((null? lis2) lis1)
        ((<? (first lis1) (first lis2))
         (cons (first lis1) (merge <? (rest lis1) lis2)))
        (else
          (cons (first lis2) (merge <? lis1 (rest lis2))))))

(define (generic<? x y)
  (define (type->number x)
    (cond ((number? x) 0)
          ((string? x) 1)
          ((symbol? x) 2)
          ((null? x)   3)
          ((list? x)   4)
          (else        5)))
  (let ((tx (type->number x))
        (ty (type->number y)))
    (cond ((< tx ty) #t)
          ((> tx ty) #f)
          (else
            (cond ((= tx 0) (< x y))
                  ((= tx 1) (string<? x y))
                  ((= tx 2) (string<? (symbol->string x)
                                      (symbol->string y)))
                  ((= tx 3) #f)
                  ((= tx 5) #f)
                  ((generic<? (first x) (first y)) #t)
                  ((generic<? (first y) (first x)) #f)
                  (else (generic<? (rest x) (rest y))))))))

(define (filter f lis)
  (let loop ((mm (map f lis)))
    (if (null? mm) mm
      (if (first mm)
        (cons (first mm) (loop (rest mm)))
        (loop (rest mm))))))

(define (pretty lis)
  (for-each (lambda (e) (display e) (newline)) lis))



(assign argl (op cons) (reg val) (reg argl))
(assign argl (op list) (reg val))
(assign continue (label after-call8))
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (n)) (reg argl) (reg env))
(assign proc (op lookup-variable-value) (const *) (reg env))
(assign val (const 0))
(assign val (const ok))
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(assign val (op compiled-procedure-entry) (reg proc))
(assign val (op lookup-variable-value) (const n) (reg env))
(assign val (op make-compiled-procedure) (label entry1) (reg env))
(branch (label false-branch4))
(goto (label after-lambda2))
(goto (reg continue))
(goto (reg val))
(perform (op define-variable!) (const fact) (reg val) (reg env))
(restore continue)
(restore env)
(restore proc)
(save continue)
(save env)
(save proc)
(test (op false?) (reg val))
(test (op primitive-procedure?) (reg proc))

