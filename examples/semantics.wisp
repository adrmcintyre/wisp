(define (<K?> X)
  (or
    (number? X)
    (string? X)))

(define (<I?> X)
  (symbol? X))

(define (<APP?> X)
  (and
    (pair? X)
    (not (eq? (first X) 'lambda))))

(define (<LAMBDA-FIXED-ARGS?> X)
  (and
    (pair? X)
    (eq? (first X) 'lambda)
    (list? (second X))))

(define (<LAMBDA-VAR-ARGS?> X)
  (and
    (pair? X)
    (eq? (first X) 'lambda)
    (not (list? (second X)))))


;;; 7.2.3. Semantic functions

;; defer to underlying implementation
(define (<KK> X)
  X)

(define (<EE> X)
  (cond
    ((<K?> X)
     (let ((K X))
       (lambda (rho kappa)
         (<send> (<KK> K) kappa))))

    ((<I?> X)
     (let ((I X))
       (lambda (rho kappa)
         (<hold> (<lookup> rho I)
                 (<single>
                   (lambda (epsilon)
                     (if (<undefined?> epsilon) (<wrong> "undefined var")
                       (<send> epsilon kappa))))))))

    ((<APP?> X)
     (let ((E0 (first X))
           (E* (rest X)))
       (lambda (rho kappa)
         ((<EE*> (<permute> X))
                rho
                (lambda (epsilon*)
                  (lambda (epsilon*)
                    (<applicate> (first epsilon*) (rest epsilon*) kappa))
                  (<unpermute> epsilon*))))))

    ((<LAMBDA-FIXED-ARGS?> X)
     (let ((I* (second X))
           (G* (all-but-last (cddr X)))
           (E0 (last X)))
       (lambda (rho kappa)
         (lambda (sigma)
           (cond
             ((<new> sigma)
              => (lambda (new_sigma)
                   (<send>
                     (cons new_sigma
                           (lambda (epsilon* kappa1)
                             (if (= (length epsilon*) (length I*))
                               (<tievals>
                                 (lambda (alpha*)
                                   (lambda (rho1)
                                     ((<CC> G*) rho1 ((<EE> E0) rho1 kappa1))
                                     (<extends> rho I* alpha*)))
                                 epsilon*)
                               (<wrong> "wrong num args"))))
                     kappa
                     (<update> new_sigma <unspecified> sigma))))
             (else (wrong "out of memory" sigma)))))))

    ((<LAMBDA-VAR-ARGS?> X)
     (let ((I* (undotted-part (second X)))
           (I (dotted-part (second X)))
           (G* (all-but-last (cddr X)))
           (E0 (last X)))
       (lambda (rho kappa)
         (lambda (sigma)
           (cond
             ((<new> sigma)
              => (lambda (new_sigma)
                   (<send>
                     (cons new_sigma
                           (lambda (epsilon* kappa1)
                             (if (>= (length epsilon*) (length I*))
                               (<tievalsrest>
                                 (lambda (alpha*)
                                   (lambda (rho1)
                                     ((<CC> G*) rho1 ((<EE> E0) rho1 kappa1))
                                     (<extends> rho (append I* I) alpha*)))
                                 epsilon*
                                 (length I*))
                               (<wrong> "wrong too few arguments"))))
                     kappa
                     (<update> new_sigma <unspecified> sigma))))
             (else (wrong "out of memory" sigma)))))))
    
    ((<IF3?> X)
     (let ((E0 (second X))
           (E1 (third X))
           (E2 (fourth X)))
       (lambda (rho kappa)
         ((<EE> E0) rho (<single> (lambda (epsilon) (if (<truish> epsilon)
                                                      ((<EE> E1) rho kappa)
                                                      ((<EE> E2) rho kappa))))))))

    ((<IF2?> X)
     (let ((E0 (second X))
           (E1 (third X)))
       (lambda (rho kappa)
         ((<EE> E0) rho (<single> (lambda (epsilon) (if (<truish> epsilon)
                                                      ((<EE> E1) rho kappa)
                                                      (<send> <unspecified> kappa))))))))

    ((<SET!?> X)
     (let ((I (second X))
           (E (third X)))
       (lambda (rho kappa)
         ((<EE> E) rho (<single> (lambda (epsilon) (<assign> (<lookup> rho I) epsilon (<send> <unspecified> kappa))))))))))

(define (<EE*> X)
  (if (null? X)
    (lambda (rho kappa) (kappa '()))
    (let ((E0 (first X))
          (E* (rest X)))
      (lambda (rho kappa)
        ((<EE> E0) rho (<single> (lambda (epsilon0)
                                   ((<EE*> E*) rho (lambda (epsilon*)
                                                     (kappa (cons epsilon0 epsilon*)))))))))))

(define (<CC> X)
  (if (null? X)
    (lambda (rho theta) theta)
    (let ((G0 (first X))
          (G* (rest X)))
      (lambda (rho theta)
        ((<EE> G0) rho (lambda (epsilon*) ((<CC> G*) rho theta)))))))

; helpers
(define (all-but-last lis)
  (if (null? (cdr lis)) '()
    (cons (car lis) (all-but-last (cdr lis)))))

(define (undotted-part lis)
  (if (pair? lis)
    (cons (car lis) (undotted-part (cdr lis)))
    '()))

(define (dotted-part lis)
  (if (pair? lis)
    (dotted-part (cdr lis))
    lis))


;;; 7.2.4. Auxiliary functions

(define (<lookup> rho I)
  ; (rho I))
  (assoc I rho))

(define (<extends> rho I* alpha*)
  (if (eq? I* '()) rho
    (<extends> (cons (cons (car I*) (car alpha*)) rho) (cdr I*) (cdr alpha*))))

(define (<wrong> X)
  (error X))

(define (<send> epsilon kappa)
  (kappa (list epsilon)))

(define (<single> psi epsilon*)
  (if (eq? (length epsilon*) 1) (psi (car epsilon*))
    (error "wrong number of return values")))

(define <new>
  (let ((alpha 0))
    (lambda (sigma)
      (set! alpha (+ alpha 1))
      alpha)))

(define (<hold> alpha kappa sigma)
  (<send> 
