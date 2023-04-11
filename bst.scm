(define-module (bst))

(define-public (make-bst-proc less-proc)
  (lambda (command . args)
    (cond
      ((equal? command 'make)
        (bst-make))
      ((equal? command 'add!)
        (apply bst-add! (append (list less-proc) args))))))

(define (bst-make)
  (cons '() (cons '() '())))

(define-public (bst-head bst)
  (car bst))

(define-public (bst-left bst)
  (car (cdr bst)))

(define-public (bst-right bst)
  (cdr (cdr bst)))

(define-public (bst-add! less-proc bst-input k)
  (if (null? (car bst-input))
    (set-car! bst-input k)
    (let loop ((bst bst-input))
      (if (less-proc k (car bst))
        (if (null? (cadr bst))
          (set-car! (cdr bst) (cons k (cons '() '())))
          (loop (cadr bst)))
        (if (null? (cddr bst))
          (set-cdr! (cdr bst) (cons k (cons '() '())))
          (loop (cddr bst)))))))
