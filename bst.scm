(define-module (bst))

#|
A bst is a pair whose car is either the empty list or a pair (head-payload .
size) and whose cdr is a pair of bsts, empty lists or a combination thereof.

The case where the car is the empty list only occurs for a bst of size zero.

Cheat-sheet:

(car bst) returns either the empty list or a pair.

If (car bst) returns a pair:
(caar bst) returns the payload.
(cdar bst) returns the size.

(cadr bst) returns either the empty list or a bst.
(cddr bst) returns either the empty list or a bst.
|#

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
        (apply bst-delete! (append (list less-proc) args)))
      ((equal? command 'size)
        (apply bst-size (append (list less-proc) args))))))

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
          (less-proc (caaadr bst) (caar bst))
          (bst-valid? less-proc (cadr bst))))
      ; right side
      (or
        (null? (cddr bst))
        (and
          (not (less-proc (caaddr bst) (caar bst)))
          (bst-valid? less-proc (cddr bst)))))))

(define-public (bst-size less-proc bst)
  (if (null? (car bst))
    0
    (cdar bst)))

(define (inc-size-bsts! bsts)
  (let loop ((bsts bsts))
    (unless (null? bsts)
      (let ((bst (car bsts)))
        (set-cdr! (car bst) (1+ (cdar bst)))
        (loop (cdr bsts))))))

(define-public (bst-add! less-proc bst-input k)
  (if (null? (car bst-input))
    (begin
      (set-car! bst-input (cons k 1)))
    (let loop ((bst bst-input) (seen '()))
      (let ((seen (cons bst seen)))
        (if (less-proc k (caar bst))
          (if (null? (cadr bst))
            (begin
              (inc-size-bsts! seen)
              (set-car! (cdr bst) (cons (cons k 1) (cons '() '()))))
            (loop (cadr bst) seen))
          (if (null? (cddr bst))
            (begin
              (inc-size-bsts! seen)
              (set-cdr! (cdr bst) (cons (cons k 1) (cons '() '()))))
            (loop (cddr bst) seen)))))))

(define-public (bst-member less-proc bst-input k)
  (let loop ((bst bst-input))
    (if (null? (car bst))
      '()
      (if (less-proc k (caar bst))
        (if (null? (cadr bst))
          '()
          (loop (cadr bst)))
        (if (not (less-proc (caar bst) k))
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
        (caar bst)
        (loop (cadr bst))))))

(define-public (bst-max less-proc bst-input)
  (let loop ((bst bst-input))
    (if (null? (car bst))
      '()
      (if (null? (cddr bst))
        (caar bst)
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

(define-public (obj-as-string obj)
  (call-with-output-string
    (lambda (port)
      (write obj port))))

(define-public (bst-pretty-print bst-input)
  (define result-port (open-output-string))
  (define display-to-result (lambda (obj) (display obj result-port)))

  (let loop ((bst bst-input) (index 1) (parent-index 0))
    (if (null? (car bst))
      (display-to-result '())
      (begin
        (display-to-result index)
        (display-to-result " ")
        (display-to-result parent-index)
        (display-to-result " ")
        (display-to-result (car bst))
        (unless (null? (cadr bst))
          (display-to-result "\n")
          (loop (cadr bst) (+ index 1) index))
        (unless (null? (cddr bst))
          (display-to-result "\n")
          (loop (cddr bst) (+ index 2) index)))))

  (display (get-output-string result-port))
  (newline))
