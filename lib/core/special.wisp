; NOTE
;   everything in here assumes none of the primitives employed
;   have been redefined when it comes to macro-expansion time

; Starting from the builtins %define, %lambda and %macro, we boostrap our way
; up to define, define-macro, lambda, let*, let, labelled let and letrec, all
; supporting internal defines.

; A helper for expanding the body of a lambda expression.
; For now it's a no-op, but later we will replace it with
; something that can handle internal defines.
(%define core:expand-lambda-body
	(%lambda (body) body))

(%define lambda
	(%macro (formals . body)
		`(%lambda ,formals
			 ,@(core:expand-lambda-body body))))

; A simple version of define-macro. When expand-lambda-body is replaced later,
; this will in turn allow for internal defines.
(%define define-macro (%macro (name-args . body)
	`(%define ,(first name-args)
		(%macro ,(rest name-args) ,@(core:expand-lambda-body body)))))

; A define that recognises two forms:
; 		(define (<fn> <args> ...) <body> ...)
;     (define <symbol> <expr>)
(define-macro (define name-args . body)
	(if (pair? name-args)
		`(%define ,(first name-args)
			 (%lambda ,(rest name-args)
				 ,@(core:expand-lambda-body body)))
		`(%define ,name-args ,@body)))

; A helper to throw an error indicating which let form was guilty.
; who:symbol message:string [opt-detail:obj]
(define (core:syntax-error! who message . opt-detail)
	(if (null? opt-detail)
		(error (string-append (symbol->string who) ": " message))
		(error (string-append (symbol->string who) ": " message) (first opt-detail))))

; A couple of helpers for validating the binding forms in let1, let, letrec, let*.
; In what follows, we will be careful not to cause these to be invoked more than
; once.

; Check the validity of a single let binding form
(define (core:assert-let-binding! who binding)
	(or (pair? binding)
		(core:syntax-error! who "ill-formed binding" binding))
	(or (symbol? (first binding))
		(core:syntax-error! who "binding variable is not a symbol" binding))
	(if (null? (rest binding))
		(core:syntax-error! who "missing binding value" binding))
	(or (pair? (rest binding))
		(core:syntax-error! who "improper binding" binding))
	(or (null? (rest (rest binding)))
		(core:syntax-error! who "unexpected form after binding value" binding))
	#t)

; Check the validity of a list of let binding forms
(define (core:assert-let-bindings! who bindings)
	(if (pair? bindings)
		(begin
			(core:assert-let-binding! who (first bindings))
			(core:assert-let-bindings! who (rest bindings)))
		(or (null? bindings)
			(core:syntax-error! who "improper binding list"))))

; A simplified variant of let that binds a single var.
; This will make our lives easier as we work through the rest of
; the implementation.

; (let1 (<var> <expr>) <body> ...)
; => ((%lambda (<var>) <body> ...) <expr>)
(define-macro (let1 binding . body)
	(core:assert-let-binding! 'let1 binding)
	(or (pair? body) (core:syntax-error! 'let1 "missing body"))
	`((%lambda
			(,(first binding)) ,@body)
		 ,(second binding)))

; Given a list of let bindings, decomposes them into a separate list of
; variables and values. E.g. ((x 1) (y 2) (z 3)) => ((x y z) (1 2 3))
;
; As we haven't bootstrapped labelled-let yet, we use tail recursion
; instead, and require vars and vals should be '() when first called.
(define (core:decompose-let-bindings bindings vars vals)
	(if (null? bindings)
		(list (reverse vars) (reverse vals))
		(let1 (binding (first bindings))
					(core:decompose-let-bindings
						(rest bindings)
						(cons (first binding) vars)
						(cons (second binding) vals)))))

; Some helpers for when the time comes to implement internal defines.

; Recognises an internal define form.
(define (core:is-internal-define? expr)
	(and (pair? expr)
		(eq? (first expr) 'define)))

; Recognises an internal define at the head of a let or lambda body.
; (potentially more follow - we only care if at least one is present).
(define (core:has-internal-defines? body)
	(and (pair? body)
		(core:is-internal-define? (first body))))

; Implement let*, which allows later initialisation expressions to refer
; to the values of earlier vars.
;
;  (let* ((v1 e1) (v2 e2) (v3 e3)) body ...)
;  =>
;  ((%lambda (v1)
;    ((%lambda (v2)
;      ((%lambda (v3) body ...)
;       e3))
;     e2))
;   e1)

(define (core:expand-let* bindings body)
	(if (null? bindings)
		`((%lambda () ,@(core:expand-lambda-body body)))
		; we don't quite have let* yet, so we need to manually nest the bindings...
		(let1 (binding (first bindings))
					(let1 (var (first binding))
								(let1 (value (second binding))
											(if (pair? (rest bindings))
												`((%lambda (,var) ,(core:expand-let* (rest bindings) body))
													 ,value)
												`((%lambda (,(first binding)) ,@(core:expand-lambda-body body))
													 ,(second binding))))))))

(define-macro (let* bindings . body)
	(core:assert-let-bindings! 'let* bindings)
	(core:expand-let* bindings body))

; Now we define let, which supports multiple bindings.

; A helper for expanding a simple unlabelled let.
; (let ((v1 e1) (v2 e2) ...) body ...)
; => ((%lambda (v1 v2 ...) body ...) e1 e2 ...)
(define (core:expand-unlabelled-let bindings body)
	(if (null? body)
		(core:syntax-error! 'let "missing body"))
	(let1 (vars-vals (core:decompose-let-bindings bindings '() '()))
		`((%lambda ,(first vars-vals) ,@(core:expand-lambda-body body))
		  ,@(second vars-vals))))

; A helper for expanding labelled-let.
(define (core:expand-labelled-let label bindings body)
	(if (null? body)
		(core:syntax-error! 'let "missing body"))
	(let* ((vars+vals (core:decompose-let-bindings bindings '() '()))
					(vars (first vars+vals))
					(vals (second vars+vals))
					(temp (gensym))
					(expanded-body `(%lambda ,vars ,@(core:expand-lambda-body body))))
		`((%lambda (,label)
				((%lambda (,temp)
					 (set! ,label ,temp)
					 (,label ,@vals))
					,expanded-body))
			 ,(undefined))))

; Implement let, dispatching to the appropriate labelled or unlabelled expander.
;
; (let ((<var> <expr>) ...) <body> ...)
; (let <label> ((<var> <expr>) ...) <body> ...)
(define-macro (let label-or-bindings . form)
	(if (symbol? label-or-bindings)
		(if (null? form)
			(core:syntax-error! 'let "missing binding list")
			; we don't have let yet, so we abuse the power of let* instead
			(begin
				(core:assert-let-bindings! 'let (first form))
				(core:expand-labelled-let label-or-bindings (first form) (rest form))))
		(if (list? label-or-bindings)
			(begin
				(core:assert-let-bindings! 'let label-or-bindings)
				(core:expand-unlabelled-let label-or-bindings form))
			(core:syntax-error! 'let "improper binding list" label-or-bindings))))

; We are working our way up to letrec, which is needed for internal defines.

; This decomposes a list of bindings into parts suitable for implementing letrec.
;
;   <bindings> => (<vars> <undefs> <temps> <inits> <sets>)
;
; Where
;   <bindings> := ((<var> <init>) ...)
; and
;   <vars> := (<var> ...)										- list of vars to bind
;   <undefs> := (#<undefined> ...)					- list of (undefined) values to prepare each binding
;   <temps> := (<temp-var> ...)							- list of temp symbols to receive init values
;   <inits> := (<init> ...)									- list of init expressions to initialise temps
;   <sets> := ((set! <var> <temp-var>) ...) - list of set! expressions to transfer temps to bound vars
;
; E.g. (x init-x) (y init-y) (z init-z)) =>
; (
;		(z y x)
;   (#<undefined> #<undefined> #<undefined>)
;   (#_3 #_2 #_1)
;		(init-z init-y init-x)
;   ((set! x #_3) (set! y #_2) (set! z #_1))
; )
;
(define (core:decompose-letrec-bindings bindings)
	(let loop ((bindings bindings)
							(vars '()) (undefs '()) (temps '()) (inits '()) (sets '()))
		(if (null? bindings)
			(list vars undefs temps inits sets)
			(let* ((binding (first bindings))
							(var (first binding))
							(temp (gensym))
							(init (second binding)))
				(loop
					(rest bindings)
					(cons var vars)
					(cons (undefined) undefs)
					(cons temp temps)
					(cons init inits)
					(cons (list 'set! var temp) sets))))))

; Finally we can define letrec. It effectively implements the following transformation:
;
; 	(letrec ((a x) (b y) ...) body)
; 	=>
; 	(let ((a #<undefined>) (b #<undefined>) ...)
;			(let ((#_1 x) (#_2 y) ...)
;				(set! a #_1)
;				(set! b #_2)
;				...
;				(let () body)))

; This implements the actual transformation from letrec to nested lambdas.
(define (core:expand-letrec bindings body)
	(let* ((parts (core:decompose-letrec-bindings bindings))
					(vars (first parts))
					(undefs (second parts))
					(temps (third parts))
					(inits (cadddr parts))				; we don't have the fourth
					(sets (car (cddddr parts))))	; and fifth procs available
		`((%lambda (,@vars)
				((%lambda (,@temps)
					 ,@sets
					 ,@(core:expand-letrec-body body))
					,@inits))
			 ,@undefs)))

; Expands the inner body of a letrec, which may contain internal defines.
; The actual expansion of defines is handled by core:expand-lambda-body,
; to be defined shortly; it is mutually recursive with core:expand-letrec.
(define (core:expand-letrec-body body)
	(if (core:has-internal-defines? body)
		(list `((%lambda () ,@(core:expand-lambda-body body))))
		body))

; A helper for transforming a sequence of internal defines into a letrec.
;
; (lambda formals
;   (define v1 e1) ...
;   (define v2 e2) ...
;   body ...)
;
; may be expressed in the equivalent form below.
;
; (lambda formals
;   (letrec ((v1 e1) (v2 e2) ...)
;     body ...))

; Converts an internal define to a let binding:
;
; 	(define <var> <expr>)          => (<var> <expr>)
; 	(define (<fn> ...) <body> ...) => (<fn> (lambda (...) <body> ...))
(define (core:internal-define->binding def)
	(or (list? def)
		(core:syntax-error! 'define "improper internal define" def))
	(or (>= (length def) 2)
		(core:syntax-error! 'define "ill-formed internal define" def))
	(let* ((def-body (rest def))
					(lhs (first def-body)))
		(if (pair? lhs)
			(let ((lambda-name (first lhs))
						 (lambda-args (rest lhs))
						 (lambda-body (rest def-body)))
				`(,lambda-name (%lambda ,lambda-args ,@(core:expand-lambda-body lambda-body))))
			(if (= (length def-body) 2)
				(let1 (rhs (second def-body))
							`(,lhs ,rhs))
				(core:syntax-error! 'define "ill-formed internal define" def)))))

; Converts a body prefixed with zero or more internal defines into
; a let-binding list followed by the non-define tail of the body.
; When first called, <bindings> is expected to be empty.
;
; ((define <v1> <e1>) (define <v2> <e2>) <body> ...)
; => ((<v1> <e1>) (<v2> <e2>) ...) <body> ...)
(define (core:body-with-internal-defines->bindings+body body bindings)
	(if (and (pair? body)
				(core:is-internal-define? (first body)))
		(let1 (def (first body))
					(core:body-with-internal-defines->bindings+body
						(rest body)
						(cons (core:internal-define->binding def) bindings)))
		(cons (reverse bindings) body)))

; Upgrade our lambda expansion to recognise internal defines.
(set! core:expand-lambda-body
	(%lambda (body)
		(let* ((bindings+body (core:body-with-internal-defines->bindings+body body '()))
						(bindings (first bindings+body))
						(rest-body (rest bindings+body)))
			(if (null? bindings)
				body
				(list (core:expand-letrec bindings rest-body))))))

(define-macro (letrec bindings . body)
	(core:assert-let-bindings! 'letrec bindings)
	(or (pair? body)
		(core:syntax-error! 'letrec "missing body"))
	(core:expand-letrec bindings body))

