; very basic help facility

(define (help fn)
 (if (%func? fn)
  (let* 
   ((help (%func-help fn))
    (name (first help))
    (args (second help))
    (body (third help)))
   (display (string-append "(" name " " args ")")) (newline)
   (display body) (newline))
  (begin
   (display "no help found for ") (display fn) (newline))))

