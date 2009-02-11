(require "utils")

(define *db-connection* #f)


;; mysql://user:pass@localhost:port/db
(define (dsn . args)
  (let ((host  (cond ((memq host:  args) => cadr) (else "localhost")))
        (user  (cond ((memq user:  args) => cadr) (else #f)))
        (pass  (cond ((memq pass:  args) => cadr) (else #f)))
        (db    (cond ((memq db:    args) => cadr) (else #f)))
        (port  (cond ((memq port:  args) => cadr) (else 0)))
        (flags (cond ((memq flags: args) => cadr) (else 0))))
    (list host user pass db port flags)))

(define (db-connect dsn)
  (if *db-connection* (%mysql-close *db-connection*))
  (set! *db-connection* (apply %mysql-connect dsn)))

(define (db-close)
  (if *db-connection* (%mysql-close *db-connection*))
  (set! *db-connection* #f))

(define (with-db-connection dsn proc)
  (let ((old-conn *db-connection*)
        (new-conn (apply %mysql-connect dsn)))
    (dynamic-wind
      (lambda ()
        (set! *db-connection* new-conn))
      (lambda ()
        (let ((res (proc)))
          (%mysql-close new-conn)
          res))
      (lambda ()
        (set! *db-connection* old-conn)))))

(define (db-escape str)
  (%mysql-escape str *db-connection*))

(define (db-one sql)
  (let* ((res (%mysql-query sql *db-connection*))
         (row (%mysql-row res)))
    (%mysql-free res)
    (and row (car row))))

(define (db-row sql)
  (let* ((res (%mysql-query sql *db-connection*))
         (row (%mysql-row res)))
    (%mysql-free res)
    row))

(define (db-query sql)
  (%mysql-query sql *db-connection*))

;; FIXME - make tail recursive
(define (db-col sql)
  (let1 (res (%mysql-query sql *db-connection*))
    (let loop ()
      (let1 (row (%mysql-row res))
        (if row (cons (car row) (loop))
          '())))))

(define (db-all sql)
  (let1 (res (%mysql-query sql *db-connection*))
    (let loop ()
      (let1 (row (%mysql-row res))
        (if row (cons row (loop))
          '())))))

(define (db-map fn sql)
  (let1 (res (%mysql-query sql *db-connection*))
    (let loop ()
      (let1 (row (%mysql-row res))
        (if row (cons (apply fn row) (loop))
          '())))))

