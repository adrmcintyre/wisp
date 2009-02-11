(%define incer (%lambda (x) (%lambda (y) (+ x y))))
((incer 3) 4)
