(include "tests/test-date.scm")

(define (test-fns name)
  (cond
    ((equal? name 'assert-equal) (lambda (a b) (= a b)))))

(define (run-tests)
  (for-each (lambda (test-fn) (test-fn test-fns))
    (list test-ts->date)))

(run-tests)
