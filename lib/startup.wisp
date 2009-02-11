(%define *require-path* '("lib" "."))

(%define *required* '())

(%define %resolve-path (%lambda (name path-list)
  (if (null? path-list)
    (error (string-append "cannot resolve: " name))
    ((%lambda (path)
       (if (vector? (file-stat path)) path
         (%resolve-path name (cdr path-list))))
     (string-append (car path-list) "/" name ".wisp")))))

(%define require
  (%lambda (name)
    ((%lambda (path)
       (if path
         (if (not (member path *required*))
           (begin
             (set! *required* (append *required* (list path)))
             (load path)
             (display "loaded ") (display path) (newline) ))))
     (%resolve-path name *require-path*))))

(require "core/special")
(require "core/map")
(require "core/control")
(require "core/arith")
(require "core/vector")
(require "core/io")
(require "core/promise")

; commented out due to several issues - see file for details
;(require "core/dynamic-wind")

