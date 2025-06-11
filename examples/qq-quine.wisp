; This quasiquote expression expands to itself:

(let ((let '`(let ((let ',let))
              ,let)))
 `(let ((let ',let))
     ,let))

