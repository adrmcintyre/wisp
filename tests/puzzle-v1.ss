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

(define (piece->vector piece)
  (let ((vec (make-vector 64 #f)))
    (for-each
      (lambda (cublet)
        (vector-set! vec (+       (first  cublet)
                            (*  4 (second cublet))
                            (* 16 (third  cublet)))
                     #t))
      piece)
    vec))

(define (vector->piece vec)
  (let loop ((i 63) (res '()))
    (if (negative? i) res
      (loop (- i 1)
            (if (vector-ref vec i)
              (cons (list (modulo i 4)
                          (modulo (/ i 4) 4)
                          (/ i 16))
                    res)
              res)))))

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

(define (normalise-piece piece)
  (let ((min-x (apply min (map first piece)))
        (min-y (apply min (map second piece)))
        (min-z (apply min (map third piece))))
    (vector->piece
      (piece->vector
        (map
          (lambda (cublet)
            (list (- (first cublet)  min-x)
                  (- (second cublet) min-y)
                  (- (third cublet)  min-z)))
          piece)))))

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


(define (make-orientable-piece piece)
  (lambda (mat)
    (normalise-piece
      (map (lambda (cublet) (mat-mul-vec mat cublet))
           piece))))

(define (all-piece-orientations piece)
  (unique-members
    (map (make-orientable-piece piece) all-orientation-mats)))

(define (translate-piece piece dx dy dz)
  (map (lambda (cublet)
         (list (+ (first  cublet) dx)
               (+ (second cublet) dy)
               (+ (third  cublet) dz)))
       piece))

(define (valid-piece? piece)
  (all?
    (lambda (cublet)
      (let ((x (first  cublet))
            (y (second cublet))
            (z (third  cublet)))
        (and (>= x 0) (< x 4)
             (>= y 0) (< y 4)
             (>= z 0) (< z 4))))
    piece))

(define (all-piece-translations piece)
  (let loop ((offset 63) (res '()))
    (if (negative? offset) res
      (let ((candidate (translate-piece piece
                                        (modulo offset 4)
                                        (modulo (/ offset 4) 4)
                                        (/ offset 16))))
        (loop (- offset 1)
              (if (valid-piece? candidate)
                (cons candidate res)
                res))))))

(define (null-piece-configuration piece)
  piece)

(define (all-piece-configurations piece)
  (apply append
         (map all-piece-translations
              (all-piece-orientations piece))))

(define (configuration-fits? state configuration)
  (none? (lambda (cublet) (member cublet state))
        configuration))

(define get-configuration-name car)

(define get-configuration-geometry cdr)

(define (insert-configuration state configuration)
  (append state (get-configuration-geometry configuration)))

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

(define empty-state '())

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


