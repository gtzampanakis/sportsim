(use-modules (system repl error-handling))

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
      (let* (
          (outcome (car outcomes))
          (outcome-type (vector-ref outcome 0))
          (st (vector-ref outcome 1)))
        (when (not (null? st))
          (newline)
          (display-backtrace st (current-error-port)))
        (loop
          (cdr outcomes)
          (+ n-all 1)
          (+ n-success (if (equal? outcome-type 'success) 1 0))
          (+ n-failure (if (equal? outcome-type 'failure) 1 0))
          (+ n-error (if (equal? outcome-type 'error) 1 0)))))))

(define (run-tests)
  (let loop ((test-suite test-suite) (outcomes '()))
    (if (null? test-suite)
      (display-run-tests-results outcomes)
      (let (
          (outcome
            (let ((test-suite-fn (car test-suite)))
              (define st '())
              (call-with-error-handling
                (lambda ()
                  (test-suite-fn test-fns)
                  (vector 'success '()))
                #:on-error
                (lambda (key . args)
                  (set! st (make-stack #t)))
                #:post-error
                (lambda (key . args)
                  (vector
                    (if
                      (and
                        (not (null? args))
                        (equal? (car args) 'exception-failed-test))
                      'failure
                      'error)
                    st))))))
        (loop
          (cdr test-suite)
          (cons outcome outcomes))))))

(run-tests)
