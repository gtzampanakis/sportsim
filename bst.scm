(define-module (bst))

(define-public (make-bst-proc less-proc)
  (lambda (command . args)
    (cond
      ((equal? command 'make)
        (bst-make))
      ((equal? command 'add!)
        (apply bst-add! (append (list less-proc) args)))
      ((equal? command 'min)
        (apply bst-min (append (list less-proc) args)))
      ((equal? command 'max)
        (apply bst-max (append (list less-proc) args)))
      ((equal? command 'valid?)
        (apply bst-valid? (append (list less-proc) args)))
      ((equal? command 'member)
        (apply bst-member (append (list less-proc) args)))
      ((equal? command 'includes?)
        (apply bst-includes? (append (list less-proc) args)))
      ((equal? command 'delete!)
        (apply bst-delete! (append (list less-proc) args))))))


(define (bst-make)
  (cons '() (cons '() '())))

(define-public (bst-valid? less-proc bst)
  (if (null? (car bst))
    #t
    (and
      ; left side
      (or
        (null? (cadr bst))
        (and
          (less-proc (cadr bst) (car bst))
          (bst-valid? less-proc (cadr bst))))
      ; right side
      (or
        (null? (cddr bst))
        (and
          (not (less-proc (cddr bst) (car bst)))
          (bst-valid? less-proc (cddr bst)))))))

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

(define-public (bst-member less-proc bst-input k)
  (let loop ((bst bst-input))
    (if (null? (car bst))
      '()
      (if (less-proc k (car bst))
        (if (null? (cadr bst))
          '()
          (loop (cadr bst)))
        (if (not (less-proc (car bst) k))
          bst
          (if (null? (cddr bst))
            '()
            (loop (cddr bst))))))))

(define-public (bst-includes? less-proc bst-input k)
  (not (null? (bst-member less-proc bst-input k))))

(define-public (bst-min less-proc bst-input)
  (let loop ((bst bst-input))
    (if (null? (car bst))
      '()
      (if (null? (cadr bst))
        (car bst)
        (loop (cadr bst))))))

(define-public (bst-max less-proc bst-input)
  (let loop ((bst bst-input))
    (if (null? (car bst))
      '()
      (if (null? (cddr bst))
        (car bst)
        (loop (cddr bst))))))

(define (leaf? bst)
  (and
    (not (null? bst))
    (null? (cadr bst))
    (null? (cddr bst))))

(define-public (bst-delete! less-proc bst-input k)
  (define bst (bst-member less-proc bst-input k))
  (unless (null? bst)
    (let ((left (cadr bst)) (right (cddr bst)))
      (if (null? left)
        (if (null? right)
          (set-car! bst '())
          (let ((right-min (bst-min less-proc right)))
            (set-car! bst right-min)
            (if (leaf? right)
            ; If subbst is a leaf remove it altogether.
              (set-cdr! (cdr bst) '())
              (bst-delete! less-proc right right-min))))
        (let ((left-max (bst-max less-proc left)))
          (set-car! bst left-max)
          (if (leaf? left)
            (set-car! (cdr bst) '())
            (bst-delete! less-proc left left-max)))))))
