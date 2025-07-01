(define (with-exception-handler handler thunk)
  (let ((old-handler #f))
    (dynamic-wind
      (lambda()
        (set! old-handler (current-exception-handler))
        (set-exception-handler! handler))
      thunk
      (lambda()
        (set-exception-handler! old-handler)))))

