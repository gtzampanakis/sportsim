(define-module (tests util)
  #:export (with-random-seed))

(define-syntax with-random-seed
  (syntax-rules ()
    ((_ seed body ...)
      (begin
        (set! *random-state* (seed->random-state seed))
        body ...))))
