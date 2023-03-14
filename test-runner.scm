(use-modules (system repl error-handling))

(use-modules (tests test-date))
(use-modules (tests test-util))

(define test-suite (list
  test-ts->date
  test-list-insert
  test-gen-rand-perm
  test-gen-round-robin))

(define (raise-failed-test obj)
  (raise-exception (cons 'exception-failed-test obj)))

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
          (key (vector-ref outcome 1))
          (args (vector-ref outcome 2))
          (st (vector-ref outcome 3)))
        (when (not (null? key))
          (let ((p (current-error-port)))
            (newline p)
            (display "========= START FAILED TEST ==========" p)
            (newline p)
            (display key p)
            (newline p)
            (display args p)
            (newline p)
            (newline p)
            (display-backtrace st p)
            (display "========= END FAILED TEST ==========" p)
            (newline p)
            (newline p)))
        (loop
          (cdr outcomes)
          (+ n-all 1)
          (+ n-success (if (equal? outcome-type 'success) 1 0))
          (+ n-failure (if (equal? outcome-type 'failure) 1 0))
          (+ n-error (if (equal? outcome-type 'error) 1 0)))))))

(define test-fns
  (lambda (name . args)
    (apply
      (case name
        ((assert-eqv)
          (lambda (a b)
            (if (not (eqv? a b))
              (raise-failed-test (list name a b)))))
        ((assert-equal)
          (lambda (a b)
            (if (not (equal? a b))
              (raise-failed-test (list name a b))))))
      args)))

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
                  (vector 'success '() '() '()))
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
                    key
                    args
                    st))))))
        (loop
          (cdr test-suite)
          (cons outcome outcomes))))))

(run-tests)
