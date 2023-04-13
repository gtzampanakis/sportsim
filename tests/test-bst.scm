(define-module (tests test-bst))

(use-modules (bst))

(define-public (test-bst-valid? test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))

  (define (v)
    (test-fns 'assert-equal (bst-proc 'valid? bst) #t))
  
  (v)
  (bst-proc 'add! bst 5)
  (v)
)

(define-public (test-bst-add! test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))

  (bst-proc 'add! bst 50)
  (test-fns 'assert-equal bst '(50 () . ()))

  (bst-proc 'add! bst 25)
  (test-fns 'assert-equal (bst-left bst) '(25 ()))
  (test-fns 'assert-equal (bst-right bst) '())

  (bst-proc 'add! bst 75)
  (test-fns 'assert-equal (bst-right bst) '(75 ()))
  (test-fns 'assert-equal (bst-left bst) '(25 ()))
)

(define-public (test-bst-min test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))
  (test-fns 'assert-equal (bst-proc 'min bst) '())
  (bst-proc 'add! bst 1)
  (test-fns 'assert-equal (bst-proc 'min bst) 1)
  (bst-proc 'add! bst 2)
  (test-fns 'assert-equal (bst-proc 'min bst) 1)
  (bst-proc 'add! bst 0)
  (test-fns 'assert-equal (bst-proc 'min bst) 0)
)

(define-public (test-bst-max test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))
  (test-fns 'assert-equal (bst-proc 'max bst) '())
  (bst-proc 'add! bst 1)
  (test-fns 'assert-equal (bst-proc 'max bst) 1)
  (bst-proc 'add! bst 2)
  (test-fns 'assert-equal (bst-proc 'max bst) 2)
  (bst-proc 'add! bst 0)
  (test-fns 'assert-equal (bst-proc 'max bst) 2)
)

(define-public (test-bst-includes? test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))

  (define (bst-includes? v) (bst-proc 'includes? bst v))
  (define (assert-true v) (test-fns 'assert-equal v #t))
  (define (assert-false v) (test-fns 'assert-equal v #f))

  (assert-false (bst-includes? 1))
  (bst-proc 'add! bst 1)
  (assert-true (bst-includes? 1))
  (assert-false (bst-includes? 2))

  (bst-proc 'add! bst 2)
  (assert-true (bst-includes? 1))
  (assert-true (bst-includes? 2))

  (bst-proc 'add! bst 0)
  (assert-true (bst-includes? 0))
  (assert-true (bst-includes? 2))
  (assert-true (bst-includes? 2))

  (bst-proc 'add! bst 3)
  (assert-true (bst-includes? 3))

  (bst-proc 'add! bst -1)
  (assert-true (bst-includes? -1))
)
