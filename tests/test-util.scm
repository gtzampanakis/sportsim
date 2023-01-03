(include "../lib/util.scm")

(define (test-append! test-fns)
  (let (
      (assert-equal (test-fns 'assert-equal)))
    (let ((l1 '(1 4)) (l2 '(2 5 3)))
      (append! l1 l2) (assert-equal l1 '(1 4 2 5 3)))))
