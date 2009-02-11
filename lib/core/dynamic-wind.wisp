;
; FIXME 
; There are a couple of issues with this library.
;
; 1. %dynamic-wind seems to cause an extra evaluation on the return value.
;    See test-count in tests/r4rstest.wisp
;
; 2. call/cc now returns a closure instead of a continuation, breaking
;    continuation->stack-frame
;

;(error "dynamic-wind: implementation is broken")



; (dynamic-wind before thunk after)
; 
; Calls `thunk' without arguments, returning the result(s) of this call. 
; `before' and `after' are called, also without arguments, as required by the 
; following rules (note that in the absence of calls to continuations captured 
; using call-with-current-continuation the three arguments are called once 
; each, in order).
; 
; `before' is called whenever execution enters the dynamic extent of the call 
; to `thunk' and `after' is called whenever it exits that dynamic extent.
; 
; The dynamic extent of a procedure call is the period between when the call is 
; initiated and when it returns.
; 
; In Scheme, because of `call-with-current-continuation', the dynamic extent of 
; a call may not be a single, connected time period.
; 
; It is defined as follows:
; 
;     * The dynamic extent is entered when execution of the body of the called 
;       procedure begins.
;     * The dynamic extent is also entered when execution is not within the 
;       dynamic extent and a continuation is invoked that was captured (using 
;       `call-with-current-continuation') during the dynamic extent.
;     * It is exited when the called procedure returns.
;     * It is also exited when execution is within the dynamic extent and a 
;       continuation is invoked that was captured while not within the dynamic 
;       extent. 
; 
; If a second call to `dynamic-wind' occurs within the dynamic extent of the 
; call to `thunk' and then a continuation is invoked in such a way that the 
; `after's from these two invocations of `dynamic-wind' are both to be called, 
; then the `after' associated with the second (inner) call to `dynamic-wind' is 
; called first.
; 
; If a second call to `dynamic-wind' occurs within the dynamic extent of the 
; call to `thunk' and then a continuation is invoked in such a way that the 
; `before's from these two invocations of `dynamic-wind' are both to be called, 
; then the `before' associated with the first (outer) call to `dynamic-wind' is 
; called first.
; 
; If invoking a continuation requires calling the `before' from one call to 
; `dynamic-wind' and the `after' from another, then the `after' is called first.
; 
; The effect of using a captured continuation to enter or exit the dynamic 
; extent of a call to `before' or `after' is undefined. 

(define dynamic-wind #f)

(let ()

  (define old-call/cc call/cc)

  (define *here* (list #f))

  (define (reroot! there)
    (if (not (eq? *here* there))
      (begin 
        (reroot! (cdr there)) 
        (let ((before (caar there)) 
              (after (cdar there)))
          (set-car! *here* (cons after before))
          (set-cdr! *here* there)
          (set-car! there #f)
          (set-cdr! there '())
          (set! *here* there)
          (before))))) 

  (define (%call/cc proc)
    (let ((here *here*)) 
      (old-call/cc
        (lambda (cont) 
          (proc 
            (lambda results
			  (if (equal? results '(%extract-continuation%)) cont
				(begin
				  (reroot! here) 
				  (apply cont results)))))))))

; FIXME - we do not support multiple values yet
; (define (%dynamic-wind before during after) 
;   (let ((here *here*)) 
;     (reroot! (cons (cons before after) here)) 
;     (call-with-values during 
;       (lambda results
;         (reroot! here)
;         (apply values results)))))

  (define (%dynamic-wind before during after) 
    (let ((here *here*)) 
      (reroot! (cons (cons before after) here)) 
      (let ((results (during)))
        (reroot! here)
        results)))

  (set! call-with-current-continuation %call/cc)
  (set! call/cc %call/cc)
  (set! dynamic-wind %dynamic-wind))

(define continuation->stack-frame
  (let ((old-continuation->stack-frame continuation->stack-frame))
    (lambda (cont)
      (old-continuation->stack-frame (cont '%extract-continuation%)))))

