(define-module (bst))

(define-public (make-bst-proc less-proc)
  (lambda (command . args)
    (cond
      ((equal? command 'make)
        (bst-make))
      ((equal? command 'add)
        (apply bst-add (append (list less-proc) args))))))

(define (bst-make)
  '())

(define-public (bst-left bst)
  (car (cdr bst)))

(define-public (bst-right bst)
  (cdr (cdr bst)))

(define (bst-add less-proc bst k)
  (if (null? bst)
    (cons k (cons bst bst))
    (let* (
        (c (car bst))
        (u (less-proc k c))
        (lr-proc (if u bst-left bst-right)))
      (if u
        (cons c (cons (bst-add less-proc (bst-left bst) k) '()))
        (cons c (cons '() (bst-add less-proc (bst-right bst) k)))))))
