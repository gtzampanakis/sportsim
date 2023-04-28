(define-module (tests test-bst))

(use-modules (bst))

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
  ;(define bst-proc (make-bst-proc <))
  ;(define ns '(1 2 0 -1 3 -2 8 10 -5))
  ;(define bst (bst-proc 'make))
  ;(for-each
  ;  (lambda (_)
  ;    (for-each
  ;      (lambda (n)
  ;        (bst-proc 'add! bst n))
  ;      ns)
  ;    (for-each
  ;      (lambda (n)
  ;        (bst-proc 'delete! bst n)
  ;        (test-fns 'assert-equal (bst-proc 'includes? bst n) #f))
  ;      ns)
  ;    (test-fns 'assert-equal bst (bst-proc 'make)))
  ;  '(1 2))
  (define bst-proc (make-bst-proc <))
  (define bst (bst-proc 'make))
  (display bst)(newline)
  (bst-proc 'add! bst 1)
  (display bst)(newline)
  (bst-proc 'add! bst 0)
  (display bst)(newline)
  (bst-proc 'delete! bst 0)
  (display bst)(newline)
  (bst-proc 'delete! bst 1)
  (display bst)(newline)
)
