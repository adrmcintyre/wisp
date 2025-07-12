;;
;; TODO:
;;   * explicit error checking
;;
;; NOTE
;;   everything in here assumes none of the primitives employed
;;   have been redefined when it comes to macro-expansion time
;;

; (define (name args ...) body)
; (define-macro (name args ...) body)
;

(%define lambda (%macro (args . body)
	`(%lambda ,args ,@body)))

(%define define-macro (%macro (name-args . body)
	`(%define ,(car name-args)
		(%macro ,(cdr name-args) ,@body))))

; Helper
; (let1 (var val) body)
;
(define-macro (let1 binding . body)
	`((%lambda
		(,(car binding)) ,@body)
	,(cadr binding)))

(define-macro (define name-args . body)
	(if (pair? name-args)
		`(%define ,(car name-args)
			(lambda ,(cdr name-args) ,@body))
		`(%define ,name-args ,@body)))

; FIXME - use set-cdr! to avoid having to call reverse at end?
(define (core:decompose-let-bindings bindings vars vals)
	(if (null? bindings)
		(list (reverse vars) (reverse vals))
		(core:decompose-let-bindings
			(cdr bindings)
			(cons (caar bindings) vars)
			(cons (cadar bindings) vals))))

;
; normal let
;
(define-macro (core:simple-let bindings . body)
	(let1 (vars-vals (core:decompose-let-bindings bindings '() '()))
		`((lambda ,(car vars-vals) ,@body)
		  ,@(cadr vars-vals))))

(define (core:decompose-letrec-bindings bindings inits renames sets)
	(if (null? bindings)
		(list (reverse inits) (reverse renames) (reverse sets))
		(let1 (var* (gensym))
			(core:decompose-letrec-bindings
				(cdr bindings)
				(cons (list (caar bindings) (undefined)) inits)
				(cons (cons var* (cdar bindings)) renames)
				(cons (list 'set! (caar bindings) var*) sets)))))


; (letrec ((a x) (b y) ...) body)
; ==>
; (let ((a #f) (b #f) ...)
; 	(let ((a' x) (b' y) ...)
; 		(set! a a')
; 		(set! b b')
; 		...
; 		(let () body)))

(define-macro (letrec bindings . body)
	(let1 (inits-renames-sets (core:decompose-letrec-bindings bindings '() '() '()))
		`(core:simple-let ,(car inits-renames-sets)
			(core:simple-let ,(cadr inits-renames-sets)
				,@(caddr inits-renames-sets)
				(core:simple-let () ,@body)))))

;
; let
;
(define-macro (let . form)
	(if (symbol? (car form))
		; labelled let
		(core:simple-let ((name      (car form))
					   (vars+vals (core:decompose-let-bindings (cadr form) () ()))
					   (body      (cddr form)))
			`(letrec ((,name (lambda ,(car vars+vals) ,@body)))
				(,name ,@(cadr vars+vals))))
		; normal let
		`(core:simple-let ,@form)))


; (lambda formals
;   (define var val) ...
;   exp1 exp2 ...)
; 
; may be expressed in the equivalent form below.
; 
; (lambda formals
;   (letrec ((var val) ...)
;     exp1 exp2 ...))

(let ()
	(define (expand-internal-defines body defines)
		(let loop ((body body) (defines defines))
			(if (and
						(pair? body)
						(pair? (car body))
						(eq? (caar body) 'define))
				(loop
					(cdr body)
					(cons
						(if (pair? (cadar body))
							`(,(caadar body) (lambda ,(cdadar body) ,@(cddar body)))
							(cdar body))
						defines))
				(cons (reverse defines) body))))

	(set! lambda
		(%macro (formals . body)
			(let1 (defines-body (expand-internal-defines body '()))
						(if (null? (car defines-body))
							`(%lambda ,formals ,@body)
							`(%lambda ,formals (letrec ,@defines-body))))))

	(set! define-macro
		(%macro (formals . body)
			(let1 (defines-body (expand-internal-defines body '()))
						(if (null? (car defines-body))
							`(%define ,(car formals) (%macro ,(cdr formals) ,@body))
							`(%define ,(car formals) (%macro ,(cdr formals) (letrec ,@defines-body)))))))
)

;
; let*
;
(define-macro (let* bindings . body)
	(let loop ((bindings bindings))
		(if (or
					(null? bindings)
					(null? (cdr bindings)))
			`(core:simple-let ,bindings ,@body)
			`(let1 ,(car bindings)
						 ,(loop (cdr bindings))))))


