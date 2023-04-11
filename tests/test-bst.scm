(define-module (tests test-bst))

(use-modules (bst))

(define-public (test-bst test-fns)
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
