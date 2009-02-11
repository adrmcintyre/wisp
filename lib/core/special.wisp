;;
;; TODO:
;;   * explicit error checking
;;   * expand internal defines inside macros?
;;
;; NOTE
;;   everything in here assumes none of the primitives employed
;;   have been redefined when it comes to macro-expansion time
;;

; (define (name args ...) body)
; (define-macro (name args ...) body)
;

(%define lambda (%macro (args . body)
	`(%lambda ,args
		. ,body)))

(%define define-macro (%macro (name-args . body)
	`(%define ,(car name-args)
		(%macro ,(cdr name-args)
			. ,body))))

(define-macro (define name-args . body)
	(if (pair? name-args)
		`(%define ,(car name-args)
			(lambda ,(cdr name-args)
				. ,body))
		`(%define ,name-args . ,body)))

; Helper
; (let1 (var val) body)
;
(define-macro (let1 binding . body)
	`((%lambda
		(,(car binding)) . ,body)
	,(cadr binding)))

; FIXME - use set-cdr! to avoid having to call reverse at end?
(define (*decompose-let-bindings* bindings vars vals)
	(if (null? bindings)
		(list (reverse vars) (reverse vals))
		(*decompose-let-bindings*
			(cdr bindings)
			(cons (caar bindings) vars)
			(cons (cadar bindings) vals))))

;
; normal let
;
(define-macro (*simple-let* bindings . body)
	(let1 (vars-vals (*decompose-let-bindings* bindings '() '()))
		`((lambda
			,(car vars-vals) . ,body)
		. ,(cadr vars-vals))))

(define (*decompose-letrec-bindings* bindings inits renames sets)
	(if (null? bindings)
		(list (reverse inits) (reverse renames) (reverse sets))
		(let1 (var* (gensym))
			(*decompose-letrec-bindings*
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
	(let1 (inits-renames-sets (*decompose-letrec-bindings* bindings '() '() '()))
		`(*simple-let* ,(car inits-renames-sets)
			(*simple-let* ,(cadr inits-renames-sets)
				,@(caddr inits-renames-sets)
				(*simple-let* () . ,body)))))

;
; let
;
(define-macro (let . form)
	(if (symbol? (car form))
		; labelled let
		(*simple-let* ((name      (car form))
					   (vars+vals (*decompose-let-bindings* (cadr form) () ()))
					   (body      (cddr form)))
			`(letrec ((,name (lambda ,(car vars+vals) . ,body)))
				(,name . ,(cadr vars+vals))))
		; normal let
		`(*simple-let* . ,form)))


; (lambda formals
;   (define var val) ...
;   exp1 exp2 ...)
; 
; may be expressed in the equivalent form below.
; 
; (lambda formals
;   (letrec ((var val) ...)
;     exp1 exp2 ...))


(define (*expand-internal-defines* body defines)
	(if (and
			(pair? body)
			(pair? (car body))
			(eq? (caar body) 'define))
		(*expand-internal-defines*
			(cdr body)
			(cons
				(if (pair? (cadar body))
					`(,(caadar body) (lambda ,(cdadar body) . ,(cddar body)))
					(cdar body))
				defines))
		(cons (reverse defines) body)))
		
(define-macro (lambda formals . body)
	(let1 (defines-body (*expand-internal-defines* body '()))
		(if (null? (car defines-body))
			`(%lambda ,formals . ,body)
			`(%lambda ,formals (letrec ,@defines-body)))))

;
; let*
;
(define (*expand-let-star* bindings body)
	(if (or
			(null? bindings)
			(null? (cdr bindings)))
		`(*simple-let* ,bindings . ,body)
		`(let1 ,(car bindings)
			,(*expand-let-star* (cdr bindings) body))))

(define-macro (let* bindings . body)
	(*expand-let-star* bindings body))

