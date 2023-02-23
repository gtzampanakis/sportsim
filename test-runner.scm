(use-modules (tests test-date))
(use-modules (tests test-util))

(define (test-fns name)
  (cond
    ((eqv? name 'assert-eqv)
      (lambda (a b) (if (not (eqv? a b)) (raise 1))))
    ((eqv? name 'assert-equal)
      (lambda (a b) (if (not (equal? a b)) (raise 1))))))

(define (run-tests)
  (for-each (lambda (test-fn) (test-fn test-fns))
    (list test-ts->date)))

(run-tests)
