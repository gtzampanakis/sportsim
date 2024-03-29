(define-module (tests test-bst))

(use-modules (bst))

(define (list-with-inserted v k obj)
  (define l (length v))
  (define r (make-list (1+ l)))
  (list-set! r k obj)
  (let loop ((i 0))
    (when (< i (1+ l))
      (unless (= i k)
        (list-set! r i
          (list-ref v (if (> i k) (1- i) i))))
      (loop (1+ i))))
  r)

(define (for-each-comb n proc)
  (when (> n 0)
    (if (= n 1)
      (proc (list 0))
      (for-each-comb
        (1- n)
        (lambda (comb)
          (let loop ((i (1- n)))
            (when (>= i 0)
              (begin
                (proc (list-with-inserted comb i (1- n)))
                (loop (1- i))))))))))

(define-public (test-bst-valid? test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))

  (define (v)
    (test-fns 'assert-equal (bst-proc 'valid? bst) #t))
  
  (v)
  (set! bst (bst-proc 'add! bst 5))
  (v)
  (set! bst (bst-proc 'add! bst -1))
  (v)
  (set! bst (bst-proc 'add! bst 8))
  (v)
  (set! bst (bst-proc 'add! bst -2))
  (v)
)

(define-public (test-bst-min-max test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))
  (test-fns 'assert-equal (bst-proc 'min bst) '())
  (test-fns 'assert-equal (bst-proc 'min bst) '())
  (set! bst (bst-proc 'add! bst 1))
  (test-fns 'assert-equal (bst-proc 'min bst) 1)
  (test-fns 'assert-equal (bst-proc 'max bst) 1)
  (set! bst (bst-proc 'add! bst 2))
  (test-fns 'assert-equal (bst-proc 'min bst) 1)
  (test-fns 'assert-equal (bst-proc 'max bst) 2)
  (set! bst (bst-proc 'add! bst 0))
  (test-fns 'assert-equal (bst-proc 'min bst) 0)
  (test-fns 'assert-equal (bst-proc 'max bst) 2)
)

(define-public (test-bst-includes? test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))

  (define (bst-includes? v) (bst-proc 'includes? bst v))
  (define (assert-true v) (test-fns 'assert-equal v #t))
  (define (assert-false v) (test-fns 'assert-equal v #f))

  (set! bst (bst-proc 'add! bst 1))
  (assert-true (bst-includes? 1))
  (assert-false (bst-includes? 2))

  (set! bst (bst-proc 'add! bst 2))
  (assert-true (bst-includes? 1))
  (assert-true (bst-includes? 2))

  (set! bst (bst-proc 'add! bst 0))
  (assert-true (bst-includes? 0))
  (assert-true (bst-includes? 2))
  (assert-true (bst-includes? 2))

  (set! bst (bst-proc 'add! bst 3))
  (assert-true (bst-includes? 3))

  (set! bst (bst-proc 'add! bst -1))
  (assert-true (bst-includes? -1))
)

(define-public (test-bst-size test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))
  (define ns0 '(3 2 1 5 4 6 0 7 -1 5 3))
  (let loop ((ns ns0))
    (unless (null? ns)
      (set! bst (bst-proc 'add! bst (car ns)))
      (test-fns 'assert-equal
        (bst-proc 'size bst)
        (+ 1 (- (length ns0) (length ns))))
      (loop (cdr ns)))))

(define-public (test-bst-delete! test-fns)
  (define (assert-true v) (test-fns 'assert-equal v #t))
  (define (assert-false v) (test-fns 'assert-equal v #f))

  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))

  (set! bst (bst-proc 'delete! bst 0))
  (assert-true (null? bst))

  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'delete! bst 0))
  (assert-true (null? bst))

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst 1))
  (set! bst (bst-proc 'delete! bst 1))
  (test-fns 'assert-equal (cdar bst) 1)

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst -1))
  (set! bst (bst-proc 'delete! bst -1))
  (test-fns 'assert-equal (cdar bst) 1)

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst 1))
  (set! bst (bst-proc 'add! bst 2))
  (set! bst (bst-proc 'add! bst 3))
  (set! bst (bst-proc 'add! bst 4))
  (set! bst (bst-proc 'add! bst 5))
  (set! bst (bst-proc 'delete! bst 5))
  (test-fns 'assert-equal (cdar bst) 5)

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst -1))
  (set! bst (bst-proc 'add! bst -2))
  (set! bst (bst-proc 'add! bst -3))
  (set! bst (bst-proc 'add! bst -4))
  (set! bst (bst-proc 'add! bst -5))
  (set! bst (bst-proc 'delete! bst -5))
  (test-fns 'assert-equal (cdar bst) 5)

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst 1))
  (set! bst (bst-proc 'delete! bst 0))
  (test-fns 'assert-equal (cdar bst) 1)

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst 1))
  (set! bst (bst-proc 'add! bst 2))
  (set! bst (bst-proc 'delete! bst 0))
  (test-fns 'assert-equal (cdar bst) 2)

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst 1))
  (set! bst (bst-proc 'add! bst 2))
  (set! bst (bst-proc 'delete! bst 1))
  (test-fns 'assert-equal (cdar bst) 2)

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst -1))
  (set! bst (bst-proc 'add! bst -2))
  (set! bst (bst-proc 'delete! bst 0))
  (test-fns 'assert-equal (cdar bst) 2)

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst -1))
  (set! bst (bst-proc 'add! bst -2))
  (set! bst (bst-proc 'add! bst -3))
  (set! bst (bst-proc 'delete! bst 0))
  (test-fns 'assert-equal (cdar bst) 3)

  (set! bst (bst-proc 'make))
  (set! bst (bst-proc 'add! bst 0))
  (set! bst (bst-proc 'add! bst 1))
  (set! bst (bst-proc 'add! bst -2))
  (set! bst (bst-proc 'add! bst 3))
  (set! bst (bst-proc 'add! bst -4))
  (set! bst (bst-proc 'delete! bst 0))
  (test-fns 'assert-equal (cdar bst) 4)
  (set! bst (bst-proc 'delete! bst 1))
  (test-fns 'assert-equal (cdar bst) 3)
  (set! bst (bst-proc 'delete! bst -2))
  (test-fns 'assert-equal (cdar bst) 2)
  (set! bst (bst-proc 'delete! bst 3))
  (test-fns 'assert-equal (cdar bst) 1)
  (set! bst (bst-proc 'delete! bst -4))
  (test-fns 'assert-equal bst '())

  (define comb-length 3)
  (for-each-comb
    comb-length
    (lambda (comb-to-add)
      (for-each-comb
        comb-length
        (lambda (comb-to-delete)
          (define bst (bst-proc 'make))
          (for-each
            (lambda (k-to-add)
              (set! bst (bst-proc 'add! bst k-to-add))
              (bst-proc 'valid? bst))
            comb-to-add)
          (for-each
            (lambda (k-to-delete)
              (set! bst (bst-proc 'delete! bst k-to-delete))
              (bst-proc 'valid? bst))
            comb-to-delete)
          (test-fns 'assert-equal bst '())))))
)

(define-public (test-bst-as-graph-string test-fns)
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))
  (let loop ((i 0))
    (if (< i 20)
      (begin
        (set! bst (bst-proc 'add! bst i))
        (loop (1+ i)))))
  (bst-as-char-matrix bst)
)

(define-public (range n)
  (let loop ((n n) (r '()))
    (if (= n 0)
      r
      (loop (1- n) (cons (1- n) r)))))

(define-public (test-bst-for-each-in-order test-fns)
  (define less-proc <)
  (define bst (bst-make))
  (define numbers (sort (append (range 20) (range 30)) less-proc))

  (for-each
    (lambda (n)
      (set! bst (bst-add! less-proc bst n)))
    numbers)

  (for-each
    (lambda (cmp-op cmp-proc)
      (for-each
        (lambda (ordering)
          (test-fns 'assert-equal
            (bst-results-list less-proc bst ordering cmp-op 15)
            (
              (if (equal? ordering 'asc) append reverse)
              (filter (lambda (n) (cmp-proc n 15)) numbers))))
        (list 'asc 'desc)))
    (list 'lt 'lte 'gt 'gte 'eq)
    (list < <= > >= =))
)
