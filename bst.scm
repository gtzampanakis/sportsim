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
      ((equal? command 'includes?)
        (apply bst-includes? (append (list less-proc) args)))
      ((equal? command 'delete-head!)
        (apply bst-delete-head! (append (list less-proc) args))))))


(define (bst-make)
  (cons '() (cons '() '())))

(define-public (bst-head bst)
  (car bst))

(define-public (bst-left bst)
  (car (cdr bst)))

(define-public (bst-right bst)
  (cdr (cdr bst)))

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

(define-public (bst-includes? less-proc bst-input k)
  (let loop ((bst bst-input))
    (if (null? (car bst))
      #f
      (if (less-proc k (car bst))
        (if (null? (cadr bst))
          #f
          (loop (cadr bst)))
        (if (not (less-proc (car bst) k))
          #t
          (if (null? (cddr bst))
            #f
            (loop (cddr bst))))))))

;(define-public (bst-delete! less-proc bst-input k)
;  (let loop ((bst bst-input))
;    (unless (null? (car bst))
;      (if (less-proc k (car bst))
;        (unless (null? (cadr bst))
;          (loop (cadr bst)))
;        (if (less-proc (car bst) k)
;          (unless (null? (cddr bst))
;            (loop (cddr bst)))
;          ; k is equal to (car bst)

;(define-public (bst-delete-head! less-proc bst-input k)
;  (let loop ((bst bst-input))
;    (

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
