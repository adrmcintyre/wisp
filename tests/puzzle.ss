; from each list of cublets:
;     generate a list of all possible configurations as cublet-lists
;         record a triple of:
;             piece-name
;             occupancy-vector
;             adjacency-vector


(require "srfi/1")

; A cublet is a triple <x,y,z>
(define (make-cublet x y z) (list x y z))
(define cublet-x first)
(define cublet-y second)
(define cublet-z third)

; ORANGE PIECES                RED PIECES
; +-----+-----+-----+-----+    +-----+-----+-----+-----+-----+-----+-----+-----+-----+
; |cross| 1   | 2   | 3   |    | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   | 9   |
; +-----+-----+-----+-----+    +-----+-----+-----+-----+-----+-----+-----+-----+-----+
; |  1  | 1   | 21  | 1   |    | 1   | 11  | 12  | 1   | 1   | 2   | 1   | 2   | 1   |
; | 111 | 11  | 1   | 21  |    | 21  |  11 | 1   | 12  | 11  | 1   | 12  | 11  | 12  |
; |  1  |  2  | 1   |  1  |    | 1   |  1  | 1   |  +  |  11 | 11  | 1   | 1   |     |
; +-----+-----+-----+-----+    +-----+-----+-----+-----+-----+-----+-----+-----+-----+

(define piece-org-cross
  '((0 1 0) (1 0 0) (1 1 0) (1 2 0) (2 1 0)))

(define piece-assoc
  (list
    (cons 'org-1 '((0 0 0) (1 0 0) (1 1 0) (2 1 0) (2 1 1)))
    (cons 'org-2 '((0 0 0) (0 1 0) (0 0 1) (1 0 0) (2 0 0)))
    (cons 'org-3 '((0 0 0) (1 0 0) (1 1 0) (1 0 1) (2 1 0)))
    (cons 'red-1 '((0 0 0) (1 0 0) (1 1 0) (1 0 1) (2 0 0)))
    (cons 'red-2 '((0 0 0) (0 1 0) (1 1 0) (1 2 0) (2 1 0)))
    (cons 'red-3 '((0 0 0) (0 1 0) (0 1 1) (1 0 0) (2 0 0)))
    (cons 'red-4 '((0 0 0) (1 0 0) (1 1 0) (1 1 1) (2 1 1)))
    (cons 'red-5 '((0 0 0) (1 0 0) (1 1 0) (2 1 0) (2 2 0)))
    (cons 'red-6 '((0 0 0) (0 0 1) (1 0 0) (2 0 0) (2 1 0)))
    (cons 'red-7 '((0 0 0) (1 0 0) (1 1 0) (1 1 1) (2 0 0)))
    (cons 'red-8 '((0 0 0) (0 0 1) (1 0 0) (1 1 0) (2 0 0)))
    (cons 'red-9 '((0 0 0) (1 0 0) (1 1 0) (1 1 1)))))

(define (occupancy->cublets piece)
  (let loopz ((z 3))
    (if (negative? z) '()
      (let ((plane (vector-ref piece z)))
        (let loopxy ((xy 15))
          (if (negative? xy) (loopz (- z 1))
            (if (bit-set? xy plane)
              (cons (list (modulo xy 4) (/ xy 4) z)
                    (loopxy (- xy 1)))
              (loopxy (- xy 1)))))))))

(define mat-ident  '(( 1  0  0) ( 0  1  0) ( 0  0  1)))
(define mat-spin-x '(( 1  0  0) ( 0  0 -1) ( 0  1  0)))
(define mat-spin-y '(( 0  0  1) ( 0  1  0) (-1  0  0)))
(define mat-spin-z '(( 0 -1  0) ( 1  0  0) ( 0  0  1)))

(define (mat-mul-vec mat vec)
  (map
    (lambda (row)
      (+ (* (first  row) (first  vec))
         (* (second row) (second vec))
         (* (third  row) (third  vec))))
    mat))

(define (mat-transpose mat)
  (list (map first mat)
        (map second mat)
        (map third mat)))

(define (mat-mul mat1 mat2)
  (mat-transpose
    (map (lambda (col) (mat-mul-vec mat1 col)) 
         (mat-transpose mat2))))

(define mat-flip (mat-mul mat-spin-x mat-spin-x))
(define mat-spin-face mat-spin-z)
(define mat-spin-corner (mat-mul mat-spin-x mat-spin-z))

(define (normalise-cublets cublets)
  (let ((min-x (apply min (map cublet-x cublets)))
        (min-y (apply min (map cublet-y cublets)))
        (min-z (apply min (map cublet-z cublets))))
    (map
      (lambda (cublet)
        (list (- (cublet-x cublet) min-x)
              (- (cublet-y cublet) min-y)
              (- (cublet-z cublet) min-z)))
      cublets)))

(define mat-eq? equal?)

(define (generate-transforms next)
  (let loop ((tx next) (res (list mat-ident)))
    (if (mat-eq? tx mat-ident) res
      (loop (mat-mul tx next) (cons tx res)))))

(define (cross-map operator as bs)
  (apply append (map (lambda (a)
                       (map (lambda (b) (operator a b))
                            bs))
                     as)))

(define (all? pred lis)
  (let loop ((lis lis))
    (or (null? lis)
      (and (pred (first lis))
           (loop (rest lis))))))

(define (none? pred lis) (all? (lambda(e) (not (pred e))) lis))

(define (any? pred lis) (not (none? pred lis)))

(define (filter pred lis)
  (let loop ((lis lis) (res '()))
    (cond ((null? lis)         res)
          ((pred (first lis))  (loop (rest lis) (cons (first lis) res)))
          (else                (loop (rest lis) res)))))

(define (unique-members lis)
  (let loop ((lis lis) (seen '()))
    (if (null? lis) seen
      (loop (rest lis)
            (if (member (first lis) seen) seen
              (cons (first lis) seen))))))

(define all-orientation-mats
  (cross-map mat-mul
             (generate-transforms mat-flip)
             (cross-map mat-mul
                        (generate-transforms mat-spin-corner)
                        (generate-transforms mat-spin-face))))

; cublet-list -> (orientation-matrix -> cublet-list)
(define (make-orientable-cublets cublets)
  (lambda (mat)
    (normalise-cublets
      (map (lambda (cublet) (mat-mul-vec mat cublet))
           cublets))))

; cublet-list -> cublet-list list
(define (all-cublets-orientations cublets)
  (unique-members
    (map (make-orientable-cublets cublets) all-orientation-mats)))


; cublet int int int -> cublet
(define (translate-cublet cublet dx dy dz)
  (list (+ (cublet-x cublet) dx)
        (+ (cublet-y cublet) dy)
        (+ (cublet-z cublet) dz)))

; cublet-list int int int -> cublet-list
(define (translate-cublets cublets dx dy dz)
  (map (lambda (cublet)
         (list (+ (cublet-x cublet) dx)
               (+ (cublet-y cublet) dy)
               (+ (cublet-z cublet) dz)))
       cublets))

; cublet -> bool
(define (valid-cublet? cublet)
  (let ((x (cublet-x cublet))
        (y (cublet-y cublet))
        (z (cublet-z cublet)))
    (and (>= x 0) (< x 4)
         (>= y 0) (< y 4)
         (>= z 0) (< z 4))))

; cublet-list -> cublet-list list
(define (all-cublets-translations cublets)
  (let loop ((offset 63) (res '()))
    (if (negative? offset) res
      (let ((candidate (translate-cublets cublets
                                        (modulo offset 4)
                                        (modulo (/ offset 4) 4)
                                        (/ offset 16))))
        (loop (- offset 1)
              (if (all? valid-cublet? candidate)
                (cons candidate res)
                res))))))

; cublet-list -> cublet-list list
(define (null-cublets-configurations cublets)
  (list cublets))

; cublet-list -> cubet-list list
(define (all-cublets-configurations cublets)
  (apply append
         (map all-cublets-translations
              (all-cublets-orientations cublets))))

(define (cublets->occupancy cublets)
  (let ((vec (make-vector 4 0)))
    (for-each
      (lambda (cublet)
        (let ((z (cublet-z cublet)))
          (vector-set! vec z
                       (logior 
                         (vector-ref vec z)
                         (ash 1 (+ (cublet-x cublet)
                                   (* 4 (cublet-y cublet))))))))
      cublets)
    vec))

(define (occupancy-occupied? occupancy cublet)
  (and (valid-cublet? cublet)
      (bit-set? 
        (+ (cublet-x cublet)
           (* (cublet-y cublet) 4))
        (vector-ref occupancy (cublet-z cublet)))))

(define (occupancy->adjacency occupancy)
  (let loop ((offset 63) (adjacency '()))
    (if (negative? offset) (apply vector adjacency)
      (let ((x (modulo offset 4))
            (y (modulo (/ offset 4) 4))
            (z (/ offset 16)))
        (loop (- offset 1)
              (cons
                (if (occupancy-occupied? occupancy (list x y z)) #f
                  (count (lambda (cublet) (occupancy-occupied? occupancy cublet))
                         (list
                           (list (- x 1) y z)
                           (list (+ x 1) y z)
                           (list x (- y 1) z)
                           (list x (+ y 1) z)
                           (list x y (- z 1))
                           (list x y (+ z 1)))))
                adjacency))))))

; symbol cublets -> (symbol adjacency) list
(define (make-bundles name cublets)
  (map
    (lambda (configuration)
      (let* ((occupancy (cublets->occupancy configuration))
             (adjacency (occupancy->adjacency occupancy)))
        (list
          name
          adjacency)))
    (all-cublets-configurations cublets)))

; bundle -> symbol
(define bundle-name first)

; bundle -> adjacency
(define bundle-adjacency second)

; a state is just another bundle
; state bundle -> state | #f
(define (new-state state bundle fail)
  (let ((adj-state  (bundle-adjacency state))
        (adj-bundle (bundle-adjacency bundle))
        (adj-result (make-vector 64 0)))
    (let loop ((i 64))
      (if (negative? i) (list 'state adj-result)
        (let ((k-state  (vector-ref adj-state i))
              (k-bundle (vector-ref adj-bundle i)))
          (if k-state
            (if k-bundle
              (begin
                (vector-set! adj-result i (+ k-state k-bundle))
                (loop (- i 1)))
              (begin
                (vector-set! adj-result i k-state)
                (loop (- i 1))))
            (if k-bundle
              (begin
                (vector-set! adj-result i k-bundle)
                (loop (- i 1)))
              #f)))))))


(define (find-even lis succ fail)
  (let lp ((lis lis))
    (if (null? lis) (fail)
      (let ((e (first lis)))
        (if (even? e) (succ e)
          (lp (rest lis)))))))
        

(find-even '(1 3 5 7 6 9 11)
           (lambda (x) (list 'success x))
           (lambda ( ) (list 'failed)))



(define (index->cublet index)
  (make-cublet
    (modulo index 4)
    (modulo (/ index 4) 4)
    (/ index 16)))

(define (most-constrained-cublet state)
  (index->cublet
    (let ((adj (bundle-adjacency state)))
      (let loop ((i 0)
                 (best-adj 0)
                 (best-i 0))
        (if (= i 64) best-i
          (let ((this-adj (vector-ref adj i)))
            (cond ((= this-adj 5)        i)
                  ((> this-adj 5)        (loop (+ i 1) best-adj best-i))
                  ((> this-adj best-adj) (loop (+ i 1) this-adj i))
                  (else                  (loop (+ i 1) best-adj best-i)))))))))

;;;
;;; GOT HERE
;;;


(define (get-viable-configurations state configurations)
  (filter
    (lambda (configuration) (configuration-fits? state (get-configuration-geometry configuration)))
    configurations))

(define (label-configuration-list name configuration-list)
  (map
    (lambda (configuration)
      (cons name configuration))
    configuration-list))

(define (compute-configuration-lists)
  (cons
    (label-configuration-list 'org-cross (list (null-piece-configuration piece-org-cross)))
    (map
      (lambda (key-value)
        (let ((name  (car key-value))
              (piece (cdr key-value)))
          (display (list "computing configurations for " name)) (newline)
          (label-configuration-list name (all-piece-configurations piece))))
      piece-assoc)))

(define empty-state #(0 0 0 0))

(define (solve)
  (let ((state empty-state)
        (used-configurations '())
        (configuration-lists (compute-configuration-lists)))
    (seek-solution state used-configurations configuration-lists)))

(define (seek-solution state used-configurations configuration-lists)
  (if (null? configuration-lists) used-configurations
      (let loop ((configuration-choices 
            (get-viable-configurations state (first configuration-lists))))

        (if (null? configuration-choices) #f
          (or
            (seek-solution 
              (insert-configuration state (first configuration-choices))
              (cons (first configuration-choices) used-configurations)
              (rest configuration-lists))
          (loop
            (rest configuration-choices)))))))



; insert cross piece
; find most constrained cublet
; iterate over all configurations which intersect cublet
; if configuration is compatible:
;   add configuration to used list
;   remove all configurations of this piece
;     attempt to solve at next depth

; 
;(define (most-constrained-cublet
;
;
;(define (seek-solution state used-configurations configuration-lists)
;  (if (null? configuration-lists) used-configurations
;    (let ((cublet (most-constrained-cublet state)))
;      (or cublet
;
;      (let loop ((configuration-choices 
;            (get-viable-configurations state (first configuration-lists))))
;
;        (if (null? configuration-choices) #f
;          (or
;            (seek-solution 
;              (insert-configuration state (first configuration-choices))
;              (cons (first configuration-choices) used-configurations)
;              (rest configuration-lists))
;          (loop
;            (rest configuration-choices)))))))
;





