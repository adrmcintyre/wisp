; FIXME multiple values not supported currently so fake 'em
(define (values . args)
  (cons '<values> args))

; from srfi-8
; FIXME - very hackish - does not check producer produces 'values'
(define-macro (receive vars producer . body)
  `(apply (lambda ,vars . , body) (cdr ,producer)))

;;; INTERNAL HELPERS
(define-macro (srfi-1:rcons a b)
  `(begin
     (set-cdr! ,a (list ,b))
     (cdr ,a)))

(define (srfi-1:any1 p? lis)
  (let loop ((lis lis))
    (cond
      ((null? lis) #f)
      ((p? (car lis)) #t)
      (else (loop (cdr lis))))))

; builtin
; (define first car)
; (define second cadr)
; (define third caddr)
(define fourth cadddr)
(define (fifth lis)            (car (cddddr lis)))
(define (sixth lis)           (cadr (cddddr lis)))
(define (seventh lis)        (caddr (cddddr lis)))
(define (eighth lis)        (cadddr (cddddr lis)))
(define (ninth lis)    (car (cddddr (cddddr lis))))
(define (tenth lis)   (cadr (cddddr (cddddr lis))))


;;; CONSTRUCTORS

(define (xcons a b)
  (cons b a))

; FIXME not tail recursive
(define (cons* arg . args)
  (let loop ((h arg)
             (t args))
    (if (null? t) h
      (cons h (loop (car t)
                    (cdr t))))))

(define (make-list n . opt)
  (let1 (fill (if (null? opt) #f (first opt)))
    (let loop ((n n)
               (res '()))
      (if (zero? n) res
        (loop (- n 1) 
              (cons fill res))))))

(define (list-tabulate n init-proc)
	(let loop ((n (- n 1))
               (res '()))
      (if (negative? n) res
        (loop (- n 1) 
              (cons (init-proc n) res)))))

; tail recursive (but not pure functional - oh well)
(define (list-copy from)
  (if (pair? from)
    (let1 (header (list (car from)))
      (let loop ((from (cdr from)) 
                 (tail-pair header))
        (if (pair? from)
          (loop (cdr from)
                (srfi-1:rcons tail-pair (car from)))
          (set-cdr! tail-pair from))
        header))
    from))

; (circular-list 1 2) => (1 2 1 2 1 2 ...)
(define (circular-list . elts)
  (if (pair? elts)
    (let1 (header (list (car elts)))
      (let loop ((elts (cdr elts)) 
                 (tail-pair header))
        (if (pair? elts)
          (loop (cdr elts)
                (srfi-1:rcons tail-pair (car elts)))
          (set-cdr! tail-pair header))
        header))
    elts))

; FIXME not tail recursive
(define (iota count . opt)
  (let ((start (if (pair? opt) (first opt) 0))
        (step  (if (and
                     (pair? opt)
                     (pair? (cdr opt)))
                 (second opt) 1)))
    (let loop ((count count)
               (elt start))
      (if (zero? count) '()
        (cons elt (loop (- count 1)
                        (+ elt step)))))))


; returns true iff lis = nil, or (cons h t) where (proper-list? t)
; circular-lists are not proper
(define (proper-list? lis)
  (if (pair? lis)
    (let loop ((turtle lis)
               (lis (cdr lis)))
      (cond ((eq? turtle lis) #f)
            ((pair? lis)
             (let1 (lis (cdr lis))
               (cond ((eq? turtle lis) #f)
                     ((pair? lis) (loop (cdr turtle)
                                        (cdr lis)))
                     (else (null? lis)))))
            (else (null? lis))))
    (null? lis)))

(define (circular-list? lis)
  (if (pair? lis)
    (let loop ((turtle lis)
               (lis (cdr lis)))
      (cond ((eq? turtle lis) #t)
            ((pair? lis)
             (let1 (lis (cdr lis))
               (cond ((eq? turtle lis) #t)
                     ((pair? lis) (loop (cdr turtle)
                                        (cdr lis)))
                     (else #f))))
            (else #f)))
    #f))

(define (dotted-list? lis)
	(if (pair? lis)
      (let loop ((turtle lis)
                 (lis (cdr lis)))
        (cond ((eq? turtle lis) #f)
              ((pair? lis)
               (let1 (lis (cdr lis))
                 (cond ((eq? turtle lis) #f)
                       ((pair? lis) (loop (cdr turtle)
                                          (cdr lis)))
                       (else (not (null? lis))))))
              (else (not (null? lis)))))
      (not (null? lis))))

; error if not passed a proper or circular list (?!)
(define (null-list? lis)
  (cond ((null? lis) #t)
        ((pair? lis) #f)
        (else (error "expected <proper-list>"))
        ))

(define (not-pair? lis)
  (not (pair? lis)))

; only defined on proper-lists
(define (list= elt= . args)
  (or
    (null? args)
    (let arg-loop ((arg1 (car args))
                   (args (cdr args)))
      (or
        (null? args)
        (and
          (let elt-loop ((arg1 arg1)
                         (arg2 (car args)))
            (or
              (and
                (null? arg1)
                (null? arg2))
              (and
                (pair? arg1)
                (pair? arg2)
                (elt= (car arg1)
                      (car arg2))
                (elt-loop (cdr arg1)
                          (cdr arg2)))))
          (arg-loop (car args)
                    (cdr args)))))))
				
;;; SELECTORS

(define (car+cdr p)
  (values (car p) (cdr p)))

; FIXME not tail-recursive
(define (take x i)
  (if (zero? i) '()
    (cons (car x) (take (cdr x) (- i 1)))))

(define (drop x i)
  (if (zero? i) x
    (drop (cdr x) (- i 1))))

(define (take-right x i)
  (let loop ((len 0) (lis x))
    (if (pair? lis)
      (loop (+ len 1) (cdr lis))
      (drop x (- len i)))))

(define (drop-right x i)
  (let loop ((len 0) (lis x))
    (if (pair? lis)
      (loop (+ len 1) (cdr lis))
      (take x (- len i)))))

; linear update versions (*may* alter args)
(define take! take)
(define drop! drop)
(define take-right! take-right)
(define drop-right! drop-right)

(define (split-at x i)
  (let loop ((x x) (i i))
    (if (zero? i) (values '() x)
      (receive (prefix suffix) (loop (cdr x) (- i 1))
               (values (cons (car x) prefix) suffix)))))

(define (last-pair x)
  (if (pair? x)
    (let loop ((x x))
      (if (pair? (cdr x))
        (loop (cdr x))
        x))
    (error "expected <pair>")))

(define (last x)
  (car (last-pair x)))

;;; MISCELLANEOUS

; builtin
; length

(define (length+ x)
  (if (pair? x)
    (let loop ((hare (cdr x))
               (tortoise x) 
               (n 1))
      (cond
        ((eq? hare tortoise) #f)
        ((pair? hare)
         (let ((hare (cdr hare))
               (tortoise (cdr tortoise))
               (n (+ n 1)))
           (cond
             ((eq? hare tortoise) #f)
             ((pair? hare)
              (loop (cdr hare) tortoise (+ n 1)))
             (else n))))
        (else n)))
    0))

(define (concatenate list-of-lists)
  (apply append list-of-lists))

; equiv (append (reverse rev-head) tail))
(define (append-reverse rev-head tail)
  (if (null? rev-head) tail
    (append-reverse (cdr rev-head) (cons (car rev-head) tail))))

; linear update
(define append! append)
(define concatenate! concatenate)
(define reverse! reverse)
(define append-reverse! append-reverse)


; zip '(one two three) 
;      '(1 2 3)
;      '(odd even odd even odd even odd even))
;     => ((one 1 odd) (two 2 even) (three 3 odd))
; 
; (zip '(1 2 3)) => ((1) (2) (3))

; FIXME
; map is not a builtin, so not very efficient
(define (zip lis . lists)
    (if (null? lists)
      ; fast case
      (srfi-1:map1 list lis)
      ; slow multi-arg case
      (let loop ((lists (cons lis lists)))
        (if (srfi-1:any1 null? lists) '()
          (cons
            (srfi-1:map1 car lists)
            (loop (srfi-1:map1 cdr lists)))))))

(define (unzip1 . lists)
  (srfi-1:map1 first lists))

(define (unzip2 . lists)
  (list (srfi-1:map1 first lists)
        (srfi-1:map1 second lists)))

(define (unzip3 . lists)
  (list (srfi-1:map1 first lists)
        (srfi-1:map1 second lists)
        (srfi-1:map1 third lists)))

(define (unzip4 . lists)
  (list (srfi-1:map1 first lists)
        (srfi-1:map1 second lists)
        (srfi-1:map1 third lists)
        (srfi-1:map1 fourth lists)))

(define (unzip5 . lists)
  (list (srfi-1:map1 first lists)
        (srfi-1:map1 second lists)
        (srfi-1:map1 third lists)
        (srfi-1:map1 fourth lists)
        (srfi-1:map1 fifth lists)))

(define (count p? lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis) (n 0))
      (if (null? lis) n
        (loop
          (cdr lis)
          (if (p? (car lis)) (+ n 1) n))))
    ; slow multi-arg case
    (let loop ((lists (cons lis lists)) (n 0))
      (if (srfi-1:any1 null? lists) n
        (loop
          (srfi-1:map1 cdr lists)
          (if (apply p? (srfi-1:map1 car lists)) (+ n 1) n))))))


;;; FOLD, UNFOLD & MAP

(define (fold kons knil lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis) (res knil))
      (if (null? lis) res
        (loop (cdr lis) (kons (car lis) res))))
    ; slow multi-arg case
    (let loop ((lists (cons lis lists)) (res knil))
      (define (mapcar l e)
        (let lp2 ((l l))
          (if (null? l) (list e)
            (cons (caar l) (lp2 (cdr l))))))
      (if (srfi-1:any1 null? lists) res
        (loop
          (srfi-1:map1 cdr lists) 
          (apply kons (mapcar lists res)))))))

(define (fold-right kons knil lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis))
      (if (null? lis) knil
        (kons (car lis) (loop (cdr lis)))))
    ; slow multi-arg case
    (let loop ((lists (cons lis lists)))
      (define (mapcar l e)
        (let lp2 ((l l))
          (if (null? l) (list e)
            (cons (caar l) (lp2 (cdr l))))))
      (if (srfi-1:any1 null? lists) knil
        (apply kons (mapcar lists (loop (srfi-1:map1 cdr lists))))))))

; tsujigiri - try a new sword on a chance passerby

; note - kons is allowed to do destructive update (set-cdr!)
; hence the (let ((tail (cdr lis))) ... )
(define (pair-fold kons knil lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis) (res knil))
      (if (null? lis) res
        (let ((tail (cdr lis)))
          (loop tail (kons lis res)))))
    ; slow multi-arg case
    (let loop ((lists (cons lis lists)) (res knil))
      (if (srfi-1:any1 null? lists) res
        (let ((tails (srfi-1:map1 cdr lists)))
          (loop tails (apply kons (append lists (list res)))))))))

(define (pair-fold-right kons knil lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis))
      (if (null? lis) knil
        (kons lis (loop (cdr lis)))))
    ; slow multi-arg case
    (let loop ((lists (cons lis lists)))
      (if (srfi-1:any1 null? lists) knil
        (apply kons (append lists (list (loop (srfi-1:map1 cdr lists)))))))))

(define (reduce f ridentity lis)
  (if (null? lis) ridentity
    (fold f (car lis) (cdr lis))))

(define (reduce-right f ridentity lis)
  (if (null? lis) ridentity
    (let loop ((lis lis))
      (let ((tail (cdr lis)))
        (if (null? tail) (car lis)
          (f (car lis) (loop tail)))))))

(define (unfold p f g seed . tail-gen)
  (if (null? tail-gen)
    ; anamorphism
    (let loop ((seed seed))
      (if (p seed) '()
        (cons (f seed) (loop (g seed)))))
    ; apomorhpism
    (let ((tail-gen (car tail-gen)))
      (let loop ((seed seed))
        (if (p seed) (tail-gen seed)
          (cons (f seed) (loop (g seed))))))))

(define (unfold-right p f g seed . tail)
  (let loop ((seed seed)
             (res (if (null? tail) '() (car tail))))
    (if (p seed) res
      (loop (g seed) (cons (f seed) res)))))

; NOTE - might also be worth defining mapcar and mapcdr helpers
(define (srfi-1:map1 f lis)
  (if (null? lis) '()
    (let1 (header (list (f (car lis))))
      (let loop ((lis (cdr lis)) 
                 (tail-pair header))
        (if (null? lis)
          (set-cdr! tail-pair '())
          (loop (cdr lis)
                (srfi-1:rcons tail-pair (f (car lis)))))
        header))))

(define (map f lis . lists)
  (if (null? lists)
    ; fast case
    (srfi-1:map1 f lis)
    ; slow multi-arg case
    (let ((lists (cons lis lists)))
      (if (srfi-1:any1 null? lists) '()
        (let ((header (list (apply f (srfi-1:map1 car lists)))))
          (let loop ((lists (srfi-1:map1 cdr lists))
                     (tail-pair header))
            (if (srfi-1:any1 null? lists)
              (set-cdr! tail-pair '())
              (loop (srfi-1:map1 cdr lists)
                    (srfi-1:rcons tail-pair (apply f (srfi-1:map1 car lists)))))
            header))))))

(define (for-each proc lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis))
      (if (pair? lis)
        (begin
          (proc (car lis))
          (loop (cdr lis)))))
    ; slow multi-arg case
    (let loop ((lists (cons lis lists)))
      (if (not (srfi-1:any1 null? lists))
        (begin
            (apply proc (srfi-1:map1 car lists))
            (loop (srfi-1:map1 cdr lists)))))))

; FIXME - make this more efficient!
(define (append-map . args)
  (apply append (apply map args)))

; linear update
(define append-map! map)
(define map! map)

; guaranteed to apply func to elements in left to right order
(define map-in-order map)

; proc is allowed to call set-cdr!
(define (pair-for-each proc lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis))
      (if (pair? lis)
        ; this let is needed because proc is allowed to set-cdr!
        (let ((tail (cdr lis)))
          (proc lis)
          (loop tail))))
    ; slow multi-arg case
    (let loop ((lists (cons lis lists)))
      (if (not (srfi-1:any1 null? lists))
        ; this let is needed because proc is allowed to set-cdr!
        (let ((tails (srfi-1:map1 cdr lists)))
            (apply proc lists)
            (loop tails))))))

; FIXME not tail-recursive
(define (filter-map f lis)
  (let loop ((lis lis))
    (if (null? lis) '()
      (receive
        (head tail) (car+cdr lis)
        (let ((e (f head)))
          (if e (cons e (loop tail))
            (loop tail)))))))


;;; FILTERING & PARTITIONING

; FIXME not tail-recursive
(define (filter p? lis)
  (let loop ((lis lis))
    (if (null? lis) '()
      (receive
        (head tail) (car+cdr lis)
        (if (p? head)
          (cons head (loop tail))
          (loop tail))))))

(define (partition p? lis)
  (let loop ((lis lis))
    (if (null? lis) (values '() '())
      (receive
        (head tail) (car+cdr lis)
        (receive
          (in out) (loop tail)
          (if (p? head)
            (values (cons head in) out)
            (values in (cons head out))))))))

(define (remove p? lis)
  (let loop ((lis lis))
    (if (null? lis) '()
      (receive
        (head tail) (car+cdr lis)
        (if (p? head) (loop tail)
          (cons head (loop tail)))))))

(define filter! filter)
(define partition! partition)
(define remove! remove)


;;; SEARCHING

(define (find p? lis)
  (let loop ((lis lis))
    (and
      (pair? lis)
      (let ((e (car lis)))
        (if (p? e) e
          (loop (cdr lis)))))))

(define (find-tail p? lis)
  (let loop ((lis lis))
    (and
      (pair? lis)
      (let ((e (car lis)))
        (if (p? e) lis
          (loop (cdr lis)))))))

(define (take-while p? lis)
  (let loop ((lis lis))
    (if (null? lis) '()
      (receive
        (head tail) (car+cdr lis)
        (if (p? head) (cons head (loop tail))
          '())))))

(define take-while! take-while)

(define (drop-while p? lis)
  (let loop ((lis lis))
    (if (null? lis) '()
      (receive
        (head tail) (car+cdr lis)
        (if (p? head) (loop tail)
          lis)))))

(define (span p? lis)
  (let loop ((lis lis))
    (if (null? lis) (values '() '())
      (receive
        (head tail) (car+cdr lis)
        (if (p? head)
          (receive
            (prefix suffix) (loop tail)
            (values (cons head prefix) suffix))
          (values '() lis))))))

(define (break p? lis)
  (let loop ((lis lis))
    (if (null? lis) (values '() '())
      (receive
        (head tail) (car+cdr lis)
        (if (p? head)
          (values '() lis)
          (receive
            (prefix suffix) (loop tail)
            (values (cons head prefix) suffix)))))))

(define span! span)
(define break! break)

(define (any p? lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis))
      (and
        (pair? lis)
        (or
          (p? (car lis))
          (loop (cdr lis)))))
    ; slow multi-arg case
    (let loop ((lists (cons lis list)))
      (and
        (not (srfi-1:any1 null? lists))
        (or
          (apply p? (srfi-1:map1 car lists))
          (loop (srfi-1:map1 cdr lists)))))))

; FIXME - should return last true value from p? if eval to non #f
(define (every p? lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis))
      (or
        (null? lis)
        (and
          (p? (car lis))
          (loop (cdr lis)))))
    ; slow multi-arg case
    (let loop ((lists (cons lis list)))
      (or
        (not (srfi-1:any1 null? lists))
        (and
          (apply p? (srfi-1:map1 car lists))
          (loop (srfi-1:map1 cdr lists)))))))
        

(define (list-index p? lis . lists)
  (if (null? lists)
    ; fast case
    (let loop ((lis lis) (i 0))
      (and
        (pair? lis)
        (if (p? (car lis)) i
          (loop (cdr lis) (+ i 1)))))
    ; slow multi-arg case
    (let loop ((lists (cons lis lists)) (i 0))
      (and
        (not (srfi-1:any1 null? lists))
        (if (apply p? (srfi-1:map1 car lists)) i
          (loop (srfi-1:map1 cdr lists) (+ i 1)))))))


(define real-member member)

(define (member x lis . opt)
  (if (null? opt) (real-member x lis)
    (let ((= (car opt)))
      (find-tail (lambda (y) (= x y)) lis))))


;;; DELETION

(define (delete x lis . opt)
  (let ((= (if (null? opt) equal? (car opt))))
    (remove (lambda (y) (= x y)) lis)))

(define delete! delete)

;; each left-most element under the equivalence class induced under =
;; is present exactly once in the output, in the same order as the input
(define (delete-duplicates lis . opt)
  (let ((= (if (null? opt) equal? (car opt))))
    (let loop ((lis lis) (res '()))
      (cond
        ((null? lis) (reverse res))
        ((member (car lis) res =)
         (loop (cdr lis) res))
        (else
          (loop (cdr lis) (cons (car lis) res)))))))

(define delete-duplicates! delete-duplicates) 


;;; ASSOCIATION LISTS
; assoc
(define real-assoc assoc)

(define (assoc key alist . opt)
  (if (null? opt) (real-assoc key alist)
    (let ((= (car opt)))
      (find (lambda(a) (= key (car a))) alist))))

; alist-cons
(define (alist-cons key datum alist)
  (cons (cons key datum) alist))

; alist-copy
(define (alist-copy alist)
  (map (lambda (a) (cons (car a) (cdr a))) alist))

; alist-delete
(define (alist-delete key alist . opt)
  (let ((= (if (null? opt) equal? (car opt))))
    (remove (lambda(a) (= key (car a))) alist)))

; alist-delete!
(define alist-delete! alist-delete)


;;; SET OPERATIONS ON LISTS
; lset<=
(define (srfi-1:lset2<= sub sup)
  (every (lambda (x) (member x sup =)) sub))

(define (lset<= = . lists)
  (or (null? lists)
      (let lp1 ((lis (car lists)) (lists (cdr lists)))
        (or (null? lists)
            (let ((sup (car lists)))
              (and
                (srfi-1:lset2<= lis sup)
                (lp1 sup (cdr lists))))))))

; lset=
(define (lset= = . lists)
  (or (null? lists)
      (let lp1 ((lis (car lists)) (lists (cdr lists)))
        (or (null? lists)
            (let ((sup (car lists)))
              (and
                (srfi-1:lset2<= lis sup)
                (srfi-1:lset2<= sup lis)
                (lp1 sup (cdr lists))))))))

; lset-adjoin
(define (lset-adjoin = lis . elts)
  (let loop ((elts elts) (res lis))
    (if (null? elts) res
      (let ((e (car elts)) (rest (cdr elts)))
        (loop rest
              (if (member e lis =) res
                (cons e res)))))))

; lset-union
(define (lset-union = . lists)
  (reduce 
    (lambda (a b)
      (if (null? a) b
        (let loop ((b b) (res a))
          (if (null? b) res
            (let ((e (car b)))
              (loop
                (cdr b)
                (if (member e res =) res
                  (cons e res))))))))
    '()
    lists))

; lset-intersection
(define (lset-intersection = lis . lists)
  (fold 
    (lambda (a b)
      (cond
        ((null? a) '())
        ((null? b) '())
        (else (filter (lambda(e) (member e b =)) a))))
    lis
    lists))

; lset-difference
(define (lset-difference = lis . lists)
  (define (del-item item lis) (delete item lis =))
  (define (del-list items lis) (fold del-item lis items))
  (fold del-list lis lists))
  
; lset-xor
(define (lset-xor = . args)
  (define (xor2 a b)
    (append
        (lset-difference = a b)
        (lset-difference = b a)))
  (fold xor2 '() args))

; FIXME - more efficient implementation is certainly possible!
; lset-diff+intersection
(define (lset-diff+intersection . args)
  (values
    (apply lset-difference args)
    (apply lset-intersection args)))

(define lset-union! lset-union)
(define lset-intersection! lset-intersection)
(define lset-difference! lset-difference)
(define lset-xor! lset-xor)
(define lset-diff+intersection! lset-diff+intersection)

