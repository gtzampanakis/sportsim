(define-module (tests test-util))

(use-modules (tests util))
(use-modules (lib util))

(define-public (test-list-insert test-fns)
  (test-fns 'assert-equal (list-insert '() 0 'obj) '(obj))
  (test-fns 'assert-equal (list-insert '() 1 'obj) '(obj))
  (test-fns 'assert-equal (list-insert '(a) 0 'obj) '(obj a))
  (test-fns 'assert-equal (list-insert '(a) 1 'obj) '(a obj))
  (test-fns 'assert-equal (list-insert '(a b) 0 'obj) '(obj a b))
  (test-fns 'assert-equal (list-insert '(a b) 1 'obj) '(a obj b))
  (test-fns 'assert-equal (list-insert '(a b) 2 'obj) '(a b obj))
  (test-fns 'assert-equal (list-insert '(a b c) 0 'obj) '(obj a b c))
  (test-fns 'assert-equal (list-insert '(a b c) 1 'obj) '(a obj b c))
  (test-fns 'assert-equal (list-insert '(a b c) 2 'obj) '(a b obj c))
  (test-fns 'assert-equal (list-insert '(a b c) 3 'obj) '(a b c obj)))

(define-public (test-gen-rand-perm test-fns)
  (with-random-seed "dc9ee38a"
    (test-fns 'assert-equal (gen-rand-perm 0) '())
    (test-fns 'assert-equal (gen-rand-perm 1) '(0))
    (test-fns 'assert-equal (gen-rand-perm 2) '(0 1))
    (test-fns 'assert-equal (gen-rand-perm 2) '(1 0))
    (test-fns 'assert-equal (gen-rand-perm 3) '(1 0 2))
    (test-fns 'assert-equal (gen-rand-perm 3) '(1 0 2))
    (test-fns 'assert-equal (gen-rand-perm 3) '(1 0 2))
    (test-fns 'assert-equal (gen-rand-perm 3) '(2 1 0))
    (test-fns 'assert-equal (gen-rand-perm 3) '(1 0 2))
    (test-fns 'assert-equal (gen-rand-perm 3) '(2 0 1))
    (test-fns 'assert-equal (gen-rand-perm 3) '(0 1 2))
    (test-fns 'assert-equal (gen-rand-perm 10) '(6 1 4 9 2 0 3 7 5 8))
    (test-fns 'assert-equal (gen-rand-perm 10) '(5 4 2 1 0 7 6 8 9 3))))

(define-public (test-gen-round-robin-no-order test-fns)
  (test-fns 'assert-equal (gen-round-robin-no-order 2) '(((0 . 1))))
  (test-fns 'assert-equal (gen-round-robin-no-order 4) '(
    ((2 . 3) (0 . 1))
    ((3 . 1) (0 . 2))
    ((1 . 2) (0 . 3))
  ))
  (test-fns 'assert-equal (gen-round-robin-no-order 6) '(
    ((3 . 4) (2 . 5) (0 . 1))
    ((4 . 5) (3 . 1) (0 . 2))
    ((5 . 1) (4 . 2) (0 . 3))
    ((1 . 2) (5 . 3) (0 . 4))
    ((2 . 3) (1 . 4) (0 . 5))
  ))
)
