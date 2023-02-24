(define-module (tests util)
  #:export (with-random-state))

(define-syntax with-random-state
  (syntax-rules ()
    ((_ state body ...)
      (begin body ...))))
