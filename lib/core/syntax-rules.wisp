; Adapted from:
;   Hygienic R5RS macro-by-example for SILK ;-*-scheme-*-
;   Dorai Sitaram ds26@gte.com http://www.cs.rice.edu/~dorai/
;   April 17, 1998

; some common Scheme utilities

(define (core:reverse! s)
    (let loop ((s s) (r '()))
      (if (null? s) r
          (let ((d (cdr s)))
            (set-cdr! s r)
            (loop d s)))))

(define (core:append! l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else (let loop ((l1 l1))
                  (if (null? (cdr l1))
                      (set-cdr! l1 l2)
                      (loop (cdr l1))))
                l1)))

(define (core:ormap f l)
    (let loop ((l l))
      (if (null? l) #f
          (or (f (car l)) (loop (cdr l))))))

(define (core:andmap f l)
    (let loop ((l l))
      (if (null? l) #t
          (and (f (car l)) (loop (cdr l))))))

; Hygiene

(define (hyg:rassq k al)
    (let loop ((al al))
      (if (null? al) #f
        (let ((c (car al)))
          (if (eq? (cdr c) k) c
            (loop (cdr al)))))))

(define (hyg:tag e kk al)
    (cond ((pair? e)
            (let* ((a-te-al (hyg:tag (car e) kk al))
                    (d-te-al (hyg:tag (cdr e) kk (cdr a-te-al))))
              (cons (cons (car a-te-al) (car d-te-al))
                (cdr d-te-al))))
      ((vector? e)
        (list->vector
          (hyg:tag (vector->list e) kk al)))
      ((symbol? e)
        (cond ((eq? e '...) (cons '... al))
          ((memq e kk) (cons e al))
          ((hyg:rassq e al) =>
            (lambda (c)
              (cons (car c) al)))
          (else
            (let ((te (gensym)))
              (cons te (cons (cons te e) al))))))
      (else (cons e al))))

; untagging

(define (hyg:untag e al tmps)
    (if (pair? e)
      (let ((a (hyg:untag (car e) al tmps)))
        (if (list? e)
          (case a
            ((quote) (hyg:untag-no-tags e al))
            ((if begin)
              `(,a ,@(map (lambda (e1)
                            (hyg:untag e1 al tmps)) (cdr e))))
            ((set! define)
              `(,a ,(hyg:untag-vanilla (cadr e) al tmps)
                 ,@(map (lambda (e1)
                          (hyg:untag e1 al tmps)) (cddr e))))
            ((lambda) (hyg:untag-lambda (cadr e) (cddr e) al tmps))
            ((letrec) (hyg:untag-letrec (cadr e) (cddr e) al tmps))
            ((let)
              (let ((e2 (cadr e)))
                (if (symbol? e2)
                  (hyg:untag-named-let e2 (caddr e) (cdddr e) al tmps)
                  (hyg:untag-let e2 (cddr e) al tmps))))
            ((let*) (hyg:untag-let* (cadr e) (cddr e) al tmps))
            ((do) (hyg:untag-do (cadr e) (caddr e) (cdddr e) al tmps))
            ((case)
              `(case ,(hyg:untag-vanilla (cadr e) al tmps)
                 ,@(map
                     (lambda (c)
                       `(,(hyg:untag-vanilla (car c) al tmps)
                          ,@(hyg:untag-list (cdr c) al tmps)))
                     (cddr e))))
            ((cond)
              `(cond ,@(map
                         (lambda (c)
                           (hyg:untag-list c al tmps))
                         (cdr e))))
            (else (cons a (hyg:untag-list (cdr e) al tmps))))
          (cons a (hyg:untag-list* (cdr e) al tmps))))
      (hyg:untag-vanilla e al tmps)))

(define (hyg:untag-list ee al tmps)
    (map (lambda (e)
           (hyg:untag e al tmps)) ee))

(define( hyg:untag-list* ee al tmps)
    (let loop ((ee ee))
      (if (pair? ee)
        (cons (hyg:untag (car ee) al tmps)
          (loop (cdr ee)))
        (hyg:untag ee al tmps))))

(define (hyg:untag-no-tags e al)
    (cond ((pair? e)
            (cons (hyg:untag-no-tags (car e) al)
              (hyg:untag-no-tags (cdr e) al)))
      ((vector? e)
        (list->vector
          (hyg:untag-no-tags (vector->list e) al)))
      ((not (symbol? e)) e)
      ((assq e al) => cdr)
      (else e)))

(define (hyg:untag-lambda bvv body al tmps)
    (let ((tmps2 (core:append! (hyg:flatten bvv) tmps)))
      `(lambda ,bvv
         ,@(hyg:untag-list body al tmps2))))

(define (hyg:untag-letrec varvals body al tmps)
    (let ((tmps (core:append! (map car varvals) tmps)))
      `(letrec
         ,(map
            (lambda (varval)
              `(,(car varval)
                 ,(hyg:untag (cadr varval) al tmps)))
            varvals)
         ,@(hyg:untag-list body al tmps))))

(define (hyg:untag-let varvals body al tmps)
    (let ((tmps2 (core:append! (map car varvals) tmps)))
      `(let
         ,(map
             (lambda (varval)
               `(,(car varval)
                  ,(hyg:untag (cadr varval) al tmps)))
             varvals)
         ,@(hyg:untag-list body al tmps2))))

(define (hyg:untag-named-let lname varvals body al tmps)
    (let ((tmps2 (cons lname (core:append! (map car varvals) tmps))))
      `(let ,lname
         ,(map
             (lambda (varval)
               `(,(car varval)
                  ,(hyg:untag (cadr varval) al tmps)))
             varvals)
         ,@(hyg:untag-list body al tmps2))))

(define (hyg:untag-let* varvals body al tmps)
    (let ((tmps2 (core:append! (core:reverse! (map car varvals)) tmps)))
      `(let*
         ,(let loop ((varvals varvals)
                      (i (length varvals)))
            (if (null? varvals) '()
              (let ((varval (car varvals)))
                (cons `(,(car varval)
                         ,(hyg:untag (cadr varval)
                            al (list-tail tmps2 i)))
                  (loop (cdr varvals) (- i 1))))))
         ,@(hyg:untag-list body al tmps2))))

(define (hyg:untag-do varinistps exit-test body al tmps)
    (let ((tmps2 (core:append! (map car varinistps) tmps)))
      `(do
         ,(map
            (lambda (varinistp)
              (let ((var (car varinistp)))
                `(,var ,@(hyg:untag-list (cdr varinistp) al
                           (cons var tmps)))))
            varinistps)
         ,(hyg:untag-list exit-test al tmps2)
         ,@(hyg:untag-list body al tmps2))))

(define (hyg:untag-vanilla e al tmps)
    (cond ((pair? e)
            (cons (hyg:untag-vanilla (car e) al tmps)
              (hyg:untag-vanilla (cdr e) al tmps)))
      ((vector? e)
        (list->vector
          (hyg:untag-vanilla (vector->list e) al tmps)))
      ((not (symbol? e)) e)
      ((memq e tmps) e)
      ((assq e al) => cdr)
      (else e)))

(define (hyg:flatten e)
    (let loop ((e e) (r '()))
      (cond ((pair? e) (loop (car e)
			     (loop (cdr e) r)))
	    ((null? e) r)
	    (else (cons e r)))))

; End of hygiene filter.

; finds the leftmost index of list l where something equal to x occurs

(define (core:position x l)
    (let loop ((l l) (i 0))
      (cond ((not (pair? l)) #f)
	    ((equal? (car l) x) i)
	    (else (loop (cdr l) (+ i 1))))))

; tests if expression e matches pattern p where k is the list of keywords

(define (core:matches-pattern? p e k)
    (cond ((core:ellipsis? p)
	   (and (or (null? e) (pair? e))
		(let* ((p-head (car p))
		       (p-tail (cddr p))
		       (e-head=e-tail (core:split-at-ellipsis e p-tail)))
		  (and e-head=e-tail
		       (let ((e-head (car e-head=e-tail))
			     (e-tail (cdr e-head=e-tail)))
			 (and (core:andmap
			       (lambda (x) (core:matches-pattern? p-head x k))
			       e-head)
			      (core:matches-pattern? p-tail e-tail k)))))))
	  ((pair? p)
	   (and (pair? e)
		(core:matches-pattern? (car p) (car e) k)
		(core:matches-pattern? (cdr p) (cdr e) k)))
	  ((symbol? p) (if (memq p k) (eq? p e) #t))
	  (else (equal? p e))))

; gets the bindings of pattern variables of pattern p for expression e;
; k is the list of keywords

(define (core:get-bindings p e k)
    (cond ((core:ellipsis? p)
	   (let* ((p-head (car p))
		  (p-tail (cddr p))
		  (e-head=e-tail (core:split-at-ellipsis e p-tail))
		  (e-head (car e-head=e-tail))
		  (e-tail (cdr e-head=e-tail)))
	     (cons (cons (core:get-ellipsis-nestings p-head k)
		     (map (lambda (x) (core:get-bindings p-head x k))
			  e-head))
	       (core:get-bindings p-tail e-tail k))))
	  ((pair? p)
	   (append (core:get-bindings (car p) (car e) k)
	     (core:get-bindings (cdr p) (cdr e) k)))
	  ((symbol? p)
	   (if (memq p k) '() (list (cons p e))))
	  (else '())))

; expands pattern p using environment r;
; k is the list of keywords

(define (core:expand-pattern p r k)
    (cond ((core:ellipsis? p)
	   (append (let* ((p-head (car p))
			  (nestings (core:get-ellipsis-nestings p-head k))
			  (rr (core:ellipsis-sub-envs nestings r)))
		     (map (lambda (r1)
			    (core:expand-pattern p-head (append r1 r) k))
			  rr))
	     (core:expand-pattern (cddr p) r k)))
	  ((pair? p)
	   (cons (core:expand-pattern (car p) r k)
	     (core:expand-pattern (cdr p) r k)))
	  ((symbol? p)
	   (if (memq p k) p
	     (let ((x (assq p r)))
	       (if x (cdr x) p))))
	  (else p)))

; returns a list that nests a pattern variable as deeply as it is ellipsed

(define (core:get-ellipsis-nestings p k)
    (let sub ((p p))
      (cond ((core:ellipsis? p) (cons (sub (car p)) (sub (cddr p))))
	    ((pair? p) (append (sub (car p)) (sub (cdr p))))
	    ((symbol? p) (if (memq p k) '() (list p)))
	    (else '()))))

; finds the subenvironments in r corresponding to the ellipsed variables in nestings

(define (core:ellipsis-sub-envs nestings r)
    (core:ormap (lambda (c)
		    (if (core:contained-in? nestings (car c)) (cdr c) #f))
		  r))

; checks if nestings v and y have an intersection

(define (core:contained-in? v y)
    (if (or (symbol? v) (symbol? y)) (eq? v y)
	(core:ormap (lambda (v_i)
			(core:ormap (lambda (y_j)
					(core:contained-in? v_i y_j))
				      y))
		      v)))

; split expression e so that its second half matches with pattern p-tail

(define (core:split-at-ellipsis e p-tail)
    (if (null? p-tail) (cons e '())
      (let ((i (core:position (car p-tail) e)))
	(if i (cons (butlast e (- (length e) i))
		    (list-tail e i))
	    (error 'core:split-at-ellipsis 'bad-arg)))))

; tests if x is an ellipsing pattern, i.e., of the form (blah ... . blah2)

(define (core:ellipsis? x)
    (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '...)))


; syntax-rules is a macro that takes
;      keyword - a list of keywords
;      clauses - zero(!) or more pattern+transformer clauses
;
; E.g. (syntax-rules ()
;          (<pattern> <transformer>)
;          (<pattern> <transformer>)
;          ...)
; Where the head of each <pattern> is the macro's keyword
; (it should be the same for each <pattern>).
; TODO - is this actually checked - maybe we can use _ for subsequent keywords?
;
;      (define-syntax foo (syntax-rules (<kw> ...) ((<kw> <pat-tail>) <transformer>) ...))
;
; is expanded to:
;
;      (define foo (%macro __syntax-rules-arg__ <syntax-rules-proc>))
;
; Where syntax-rules-proc is computed by
;
;      (core:syntax-rules-proc
;          '<kw>
;          '(<kw> ...) 
;          '(((<kw> <pat-tail>) <transformer>) ...)
;          __syntax-rules-arg__
;          __syntax-rules-keywords__)
;
(define-macro (syntax-rules keywords . clauses)
    (let ((macro-name (caaar clauses)))
      `(%macro __syntax-rules-arg__
         ,(core:syntax-rules-proc macro-name keywords clauses
                                 '__syntax-rules-arg__
                                 '__syntax-rules-keywords__))))

(define (core:syntax-rules-proc macro-name keywords clauses arg-sym keywords-sym)
    ; include the macro-name in the list of keywords
    (let ((keywords (cons macro-name keywords)))
      `(let ((,arg-sym (cons ',macro-name ,arg-sym))
           (,keywords-sym ',keywords))
         (cond ,@(map
                  (lambda (clause)
                    (let ((in-pattern (car clause))
                          (out-pattern (cadr clause)))
                      `((core:matches-pattern? ',in-pattern ,arg-sym
                                              ,keywords-sym)
                        (let ((tagged-out-pattern+alist
                               (hyg:tag
                                ',out-pattern
                                (core:append! (hyg:flatten ',in-pattern)
                                         ,keywords-sym) '())))
                          (hyg:untag
                           (core:expand-pattern
                            (car tagged-out-pattern+alist)
                            (core:get-bindings ',in-pattern ,arg-sym
                                              ,keywords-sym)
                            ,keywords-sym)
                           (cdr tagged-out-pattern+alist)
                           '())))))
                  clauses)
               (else (error ',macro-name 'no-matching-clause
                            ',clauses))))))

;; define-syntax, let-syntax and letrec-syntax are just aliases for define, let and letrec.
(define define-syntax (%macro args (cons 'define args)))

(define let-syntax (%macro args (cons 'let args)))

(define letrec-syntax (%macro args (cons 'letrec args)))

