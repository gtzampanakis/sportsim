(define-module (bst))

(define-public (make-bst-proc less-proc)
  (lambda (command . args)
    (cond
      ((equal? command 'make)
        (bst-make))
      ((equal? command 'add!)
        (apply bst-add! (append (list less-proc) args))))))

(define (bst-make)
  '())

(define-public (bst-head bst)
  (car bst))

(define-public (bst-left bst)
  (car (cdr bst)))

(define-public (bst-right bst)
  (cdr (cdr bst)))

(define-public (bst-add! less-proc bst-input k)
  (if (null? bst-input)
    (cons k (cons '() '()))
    (let loop ((bst bst-input))
      (if (less-proc k (car bst))
        (if (null? (cadr bst))
          (begin
            (set-car! (cdr bst) (cons k (cons '() '())))
            bst-input)
          (loop (cadr bst)))
        (if (null? (cddr bst))
          (begin
            (set-cdr! (cdr bst) (cons k (cons '() '())))
            bst-input)
          (loop (cddr bst)))))))
