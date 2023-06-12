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
  (test-fns 'assert-equal (bst-proc 'min-max bst 'min) '())
  (test-fns 'assert-equal (bst-proc 'min-max bst 'min) '())
  (set! bst (bst-proc 'add! bst 1))
  (test-fns 'assert-equal (bst-proc 'min-max bst 'min) 1)
  (test-fns 'assert-equal (bst-proc 'min-max bst 'max) 1)
  (set! bst (bst-proc 'add! bst 2))
  (test-fns 'assert-equal (bst-proc 'min-max bst 'min) 1)
  (test-fns 'assert-equal (bst-proc 'min-max bst 'max) 2)
  (set! bst (bst-proc 'add! bst 0))
  (test-fns 'assert-equal (bst-proc 'min-max bst 'min) 0)
  (test-fns 'assert-equal (bst-proc 'min-max bst 'max) 2)
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
  (set! bst (bst-proc 'add! bst 1))
  (set! bst (bst-proc 'add! bst 2))
  (set! bst (bst-proc 'add! bst 3))
  (set! bst (bst-proc 'add! bst 4))
  (set! bst (bst-proc 'add! bst 5))
  (set! bst (bst-proc 'add! bst 6))
  (set! bst (bst-proc 'add! bst 7))
  (set! bst (bst-proc 'add! bst 8))

  ;(display (bst-as-graph-string bst))
  ;(display (bst-as-box-string bst))
  ;(display (obj-as-char-matrix '(1 2 3)))(newline)
  ;(display-char-matrix (bst-as-char-matrix bst))
  ;(display (bst-as-char-matrix bst))(newline)
  ;(display (side-by-side-char-matrices '(1 2 3) '(0 1 2 3 4)))(newline)
  ;(display
  ;  (append-with-min-sizes
  ;    (list '(1 2 3) '(4 5 6) '(7 8 9) '(2 3))
  ;    (list 7 15 22 4)))
  ;(newline)
  ;(display-char-matrix
  ;  (side-by-side-char-matrices
  ;    (list
  ;      (list #\a #\b #\z)
  ;      (list #\a #\b #\y)
  ;      (list #\t)
  ;      (list #\t))
  ;    (list
  ;      (list #\d #\e)
  ;      (list #\d)
  ;      (list #\f #\g))
  ;    (list
  ;      (list #\i #\p #\n))))
  (display-char-matrix
    (bst-as-char-matrix bst))

  (set! bst (bst-proc 'balance! bst))

  (display-char-matrix
    (bst-as-char-matrix bst))
)
