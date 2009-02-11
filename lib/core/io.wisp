
;;; CORE I/O

(define (call-with-input-file path proc)
  (let* ((port (open-input-file path))
         (res (proc port)))
    (close-input-port port)
    res))

(define (call-with-output-file path proc)
  (let* ((port (open-output-file path))
         (res (proc port)))
    (close-output-port port)
    res))

(define (with-input-from-file path proc)
  (let ((old-port (current-input-port))
        (new-port (open-input-file path)))
    (dynamic-wind
      (lambda()
        (%set-current-input-port! new-port))
      (lambda()
        (let ((res (proc)))
          (close-input-port new-port)
          res))
      (lambda()
        (%set-current-input-port! old-port)))))

(define (with-output-to-file path proc)
  (let ((old-port (current-output-port))
        (new-port (open-output-file path)))
    (dynamic-wind
      (lambda()
        (%set-current-output-port! new-port))
      (lambda()
        (let ((res (proc)))
          (close-output-port new-port)
          res))
      (lambda()
        (%set-current-output-port! old-port)))))

