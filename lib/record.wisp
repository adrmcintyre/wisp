;
; ropey implementation of records
;

(define-macro (define-record &rec &slots)
  (let ((&x (gensym))
        (&rec-str (symbol->string &rec)))
    `(begin
       ,@(let ((&make-rec (string->symbol (string-append "make-" &rec-str)))
             (&rec? (string->symbol (string-append &rec-str "?"))))
         `((define (,&make-rec ,@&slots)
             (%record ',&rec ,@&slots))
           (define (,&rec? ,&x)
             (and
               (record? ,&x)
               (eq? (%record-ref ,&x 0) ',&rec)))))
       ,@(let loop ((&slots &slots) (&i 1))
           (if (null? &slots) '()
             (let ((&rec-slot (string->symbol (string-append &rec-str "-" (symbol->string (car &slots))))))
               (cons
                 `(define (,&rec-slot ,&x) (%record-ref ,&x ,&i))
                 (loop (cdr &slots) (+ &i 1)))))))))

