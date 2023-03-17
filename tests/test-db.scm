(define-module (tests test-db))

(use-modules (tests util))
(use-modules (db))

(define-public (test-compare-values test-fns)
  (test-fns 'assert-equal (compare-values 5 8) -3)
  (test-fns 'assert-equal (compare-values "a" "b") -1)
  (test-fns 'assert-equal (compare-values "b" "a") 1)
  (test-fns 'assert-equal (compare-values "a" "a") 0))

(define-public (test-less-records test-fns)
  (define r1 #(0 "a" "b" "c"))
  (define r2 #(1 "b" "a" "c"))
  (test-fns 'assert-equal (less-records r1 r2 '()) #f)
  (test-fns 'assert-equal (less-records r1 r2 '(0 1 2)) #t)
  (test-fns 'assert-equal (less-records r1 r2 '(3)) #f)
  (test-fns 'assert-equal (less-records r1 r2 '(3 0)) #t)
  (test-fns 'assert-equal (less-records r1 r2 '(3 0 1)) #t))

(define-public (test-find-index test-fns)
  (test-fns 'assert-equal (find-index 'c '(a b c)) 2)
  (test-fns 'assert-equal (find-index 'd '(a b c)) #f))

(define-public (test-find-indices test-fns)
  (test-fns 'assert-equal (find-indices '(c a) '(a b c)) '(2 0))
  (test-fns 'assert-equal (find-indices '(d) '(a b c)) '(#f))
  (test-fns 'assert-equal (find-indices '(d a b) '(a b c)) '(#f 0 1)))
