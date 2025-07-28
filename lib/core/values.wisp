; (call-with-values producer:proc consumer:proc)
; Calls <producer> with no arguments, then invokes <consumer> with
; the returned value(s) as arguments, and returns the result.
;
; (call-with-values (lambda() (values 4 5)) (lambda(a b) b)) => 5

(define (call-with-values producer consumer)
  (apply consumer (%values->list (producer))))
