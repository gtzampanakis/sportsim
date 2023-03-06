(use-modules (tests test-date))
(use-modules (tests test-util))

(define test-suite (list
  test-ts->date
  test-list-insert))
  ;test-gen-rand-perm))

(define (raise-failed-test obj)
  (raise-exception 'exception-failed-test #:continuable? #f))

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

(define (display-run-tests-results outcomes)
  (let loop (
      (outcomes outcomes)
      (n-all 0)
      (n-success 0)
      (n-failure 0)
      (n-error 0))
    (if (null? outcomes)
      (format #t
        "Outcome: ~a\nTests: ~a Success: ~a Failure: ~a Error: ~a\n"
        (if (= n-all n-success) "PASS" "FAIL")
        n-all
        n-success
        n-failure
        n-error)
      (loop
        (cdr outcomes)
        (+ n-all 1)
        (+ n-success (if (equal? (car outcomes) 'success) 1 0))
        (+ n-failure (if (equal? (car outcomes) 'failure) 1 0))
        (+ n-error (if (equal? (car outcomes) 'error) 1 0))))))

(define (run-tests)
  (let loop ((test-suite test-suite) (outcomes '()))
    (if (null? test-suite)
      (display-run-tests-results outcomes)
      (let (
          (outcome
            (let ((test-suite-fn (car test-suite)))
              (with-exception-handler
                (lambda (exc)
                  (cond
                    ((equal? exc 'exception-failed-test) 'failure)
                    (else 'error)))
                (lambda () (test-suite-fn test-fns) 'success)
                #:unwind? #t))))
        (loop (cdr test-suite) (cons outcome outcomes))))))

(run-tests)
