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
             (display "loading ") (display path) (newline)
             (load path)))))
     (%resolve-path name *require-path*))))

(require "core/special")
(require "core/map")
(require "core/control")
(require "core/macro")
(require "core/quasiquote")
(require "core/compile")
(require "core/eval")
(require "core/dynamic-wind")
(require "core/exception")
(require "core/syntax-rules")
(require "core/output")
(require "core/reader")
(require "core/io")
(require "core/load")
; any syntax enabled by core/reader is available after this point
; for example alternate parens: [...] and {...}

(require "core/arith")
(require "core/promise")
(require "core/pretty-print")
(require "core/help")
