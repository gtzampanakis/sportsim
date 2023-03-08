(define-module (tests util)
  #:export (with-random-seed))

(define-syntax with-random-seed
  (syntax-rules ()
    ((_ seed body ...)
      (let ((kept-state *random-state*))
        (dynamic-wind
          (lambda ()
            (set! *random-state* (seed->random-state seed)))
          (lambda ()
            body ...)
          (lambda ()
            (set! *random-state* kept-state)))))))
