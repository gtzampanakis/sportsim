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

(define-public (test-gen-round-robin test-fns)
  (test-fns 'assert-equal (gen-round-robin 2) '(((0 . 1))))
  (test-fns 'assert-equal (gen-round-robin 4)
    '(
      ((2 . 3) (1 . 0))
      ((3 . 1) (2 . 0))
      ((1 . 2) (0 . 3))
  ))
  (test-fns 'assert-equal (gen-round-robin 6)
    '(
      ((3 . 4) (5 . 2) (0 . 1))
      ((4 . 5) (1 . 3) (0 . 2))
      ((5 . 1) (2 . 4) (3 . 0))
      ((1 . 2) (5 . 3) (4 . 0))
      ((2 . 3) (1 . 4) (0 . 5))
  ))
)

(define-public (test-ts->dow test-fns)
  (test-fns 'assert-equal (ts->dow 0) 3)
  (test-fns 'assert-equal (ts->dow 86399) 3)
  (test-fns 'assert-equal (ts->dow 86400) 4)
  (test-fns 'assert-equal (ts->dow 1677666758) 2)
)

(define-public (test-valid-date? test-fns)
  (test-fns 'assert-true (valid-date? (date 2000 1 1)))
  (test-fns 'assert-true (valid-date? (date 2000 2 29)))
  (test-fns 'assert-false (valid-date? (date 2001 2 29)))
  (test-fns 'assert-true (valid-date? (date 2004 2 29)))
)

(define-public (test-compare-dates test-fns)
  (test-fns 'assert-equal (compare-dates (date 2000 1 1) (date 2000 1 1)) 0)
  (test-fns 'assert-equal (compare-dates (date 2000 1 1) (date 2001 1 1)) -1)
  (test-fns 'assert-equal (compare-dates (date 2001 1 1) (date 2000 1 1)) 1)
  (test-fns 'assert-equal (compare-dates (date 2001 1 1) (date 2001 1 2)) -1)
  (test-fns 'assert-equal (compare-dates (date 2001 1 2) (date 2001 1 1)) 1)
)

(define-public (test-add-months test-fns)
  (test-fns 'assert-true
    (date=? (add-months (date 2000 1 1) 1) (date 2000 2 1)))
  (test-fns 'assert-true
    (date=? (add-months (date 2000 1 20) 1) (date 2000 2 20)))
  (test-fns 'assert-true
    (date=? (add-months (date 2000 1 1) -1) (date 1999 12 1)))
  (test-fns 'assert-true
    (date=? (add-months (date 2000 1 1) 11) (date 2000 12 1)))
  (test-fns 'assert-true
    (date=? (add-months (date 2000 1 1) 12) (date 2001 1 1)))
  (test-fns 'assert-true
    (date=? (add-months (date 2000 1 1) -12) (date 1999 1 1)))
  (test-fns 'assert-true
    (date=? (add-months (date 2000 1 1) 120) (date 2010 1 1)))
  (test-fns 'assert-equal (add-months (date 2000 1 30) 1) #f)
  (test-fns 'assert-equal (add-months (date 2001 8 31) 1) #f)
)

(define-public (test-add-years test-fns)
  (test-fns 'assert-true
    (date=? (add-years (date 2000 1 1) 1) (date 2001 1 1)))
  (test-fns 'assert-equal
    (add-years (date 2004 2 29) 1) #f)
  (test-fns 'assert-true
    (date=? (add-years (date -1 1 1) 1) (date 1 1 1)))
  (test-fns 'assert-true
    (date=? (add-years (date -10 1 1) 100) (date 91 1 1)))
)

(define-public (test-max-date-that-matches test-fns)
  (test-fns 'assert-true
    (date=? (max-date-that-matches 2000 1 1) (date 2000 1 1)))
  (test-fns 'assert-true
    (date=? (max-date-that-matches 2000 2 '()) (date 2000 2 29)))
  (test-fns 'assert-true
    (date=? (max-date-that-matches 2004 2 '()) (date 2004 2 29)))
  (test-fns 'assert-true
    (date=? (max-date-that-matches 2100 2 '()) (date 2100 2 28)))
  (test-fns 'assert-true
    (date=? (max-date-that-matches 2100 '() 29) (date 2100 12 29)))
  (test-fns 'assert-true
    (date=? (max-date-that-matches 2100 '() '()) (date 2100 12 31)))
)

(define-public (test-next-date-for-schedule test-fns)
  (define as-of-date (date 2000 6 1))
  (test-fns 'assert-true
    (date=?
      (next-date-for-schedule as-of-date '() '() '()) (date 2000 6 1)))
  (test-fns 'assert-true
    (date=?
      (next-date-for-schedule as-of-date 2000 '() '()) (date 2000 6 1)))
  (test-fns 'assert-equal
    (next-date-for-schedule as-of-date 1999 '() '()) #f)
  (test-fns 'assert-equal
    (next-date-for-schedule as-of-date 1999 6 '()) #f)
  (test-fns 'assert-true
    (date=?
      (next-date-for-schedule as-of-date '() 8 '()) (date 2000 8 1)))
)
