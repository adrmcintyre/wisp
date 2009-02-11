;
; srfi-9 compatiable records
;

; intended to support the following syntax
; (define-record-type :pare
;   (kons x y)
;   pare?
;   (x kar set-kar!)
;   (y kdr))

;;; FIXME
;;; add proper argument checking

(let()

  (define record-marker (%record))

  (define %record-type (%make-record 3))
  (%record-set! %record-type 0 %record-type)
  (%record-set! %record-type 1 '%record-type)
  (%record-set! %record-type 2 '(name field-tags))

  ; assert (all symbol? field-tags)
  (define (make-record-type name field-tags)
    (%record %record-type name field-tags))

  (define (record-type-name record-type)
    (%record-ref record-type 1))

  (define (record-type-field-tags record-type)
    (%record-ref record-type 2))

  (define (make-record-predicate rec-type)
    (lambda (x)
      (and
        (record? x)
        (eq? (%record-ref x 0) rec-type))))

  ; assert (all symbol? cons-form)
  (define (make-record-constructor rec-type cons-form)
    `(%record ,rec-type . ,(cdr cons-form)))

  ; accessor-spec ::= (tag getter)
  ;                 | (tag getter setter)
  (define (make-record-accessor rec-type accessor-spec)
    (let ((offset     (record-tag-offset rec-type (car accessor-spec)))
          (getter     (cadr accessor-spec))
          (setter-opt (cddr accessor-spec)))
      (if (null? setter-opt)
        (make-record-getter offset getter)
        `(begin
           ,(make-record-getter offset getter)
           ,(make-record-setter offset (car setter-opt))))))

  (define (make-record-getter offset getter)
    (let ((&rec (gensym)))
      `(define (,getter ,&rec)
         (%record-ref ,&rec ,offset))))

  (define (make-record-setter offset setter)
    (let ((&rec (gensym))
          (&val (gensym)))
      `(define (,setter ,&rec ,&val)
         (%record-set! ,&rec ,offset ,&val))))

  (define (record-tag-offset rec-type tag)
    (let loop ((i 1) (tags (record-type-field-tags rec-type)))
      (cond
        ((null? tags) (error "tag not in record"))
        ((eq? tag (car tags)) i)
        (else (loop (+ i 1) (cdr tags))))))

  (define-macro (define-record-type type-name cons-form pred-name . accessors)
    (let* ((rec-type (make-record-type type-name (cdr cons-form)))
           (rec-cons (make-record-constructor rec-type cons-form))
           (rec-pred (make-record-predicate rec-type))
           (rec-accs (map (lambda (accessor-spec)
                            (make-record-accessor rec-type accessor-spec))
                          accessors)))
      `(begin
         (define ,type-name ,rec-type)
         (define ,cons-form ,rec-cons)
         (define ,pred-name ,rec-pred)
         ,@
         rec-accs)))
)

