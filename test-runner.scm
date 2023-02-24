(use-modules (tests test-date))
(use-modules (tests test-util))

(define test-suite (list
  test-ts->date
  test-list-insert))
  ;test-gen-rand-perm))

(define (raise-failed-test obj)
  (display obj)
  (newline)
  (raise 1))

(define test-fns
  (lambda (name . args)
    (apply
      (case name
        ((assert-eqv)
          (lambda (a b)
            (if (not (eqv? a b)) (raise-failed-test (list a b)))))
        ((assert-equal)
          (lambda (a b)
            (if (not (equal? a b)) (raise-failed-test (list a b))))))
      args)))

(define (run-tests)
  (let loop ((test-suite test-suite) (success 0) (fail 0) (err 0))
    (if (null? test-suite)
      (list success fail err)
      (let (
          (test-suite-fn (car test-suite)))
        (with-exception-handler
            (lambda (e) )
          (test-suite-fn test-fns))
        (loop (cdr test-suite) (1+ success) fail err)))))

(run-tests)
