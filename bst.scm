(define-module (bst))

#|
A bst is a pair whose car is either the empty list or a pair (head-payload .
size) and whose cdr is a pair of bsts, empty lists or a combination thereof.

A bst is either
  the empty list or
  a pair whose
    car is a pair (payload . size) and whose
    cdr is a pair (bst-left . bst-right)

Cheat-sheet:

(caar bst) returns the payload.
(cdar bst) returns the size.

(cadr bst) returns either the empty list or a bst (left-bst).
(cddr bst) returns either the empty list or a bst (right-bst).
|#

(define-public (make-bst-proc less-proc)
  (lambda (command . args)
    (cond
      ((equal? command 'make)
        (bst-make))
      ((equal? command 'add!)
        (apply bst-add! (append (list less-proc) args)))
      ((equal? command 'min-max)
        (apply bst-min-max (append (list less-proc) args)))
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
        (apply bst-size (append (list less-proc) args)))
      ((equal? command 'walk)
        (apply bst-walk (append (list less-proc) args)))
      ((equal? command 'balance!)
        (apply bst-balance! (append (list less-proc) args))))))

(define (bst-make)
  '())

(define-public (bst-valid? less-proc bst-input)
  (let loop ((bst bst-input))
    (or
      (null? bst)
      (let
        (
          (left-bst (cadr bst))
          (right-bst (cddr bst))
          (payload (caar bst))
          (size (cdar bst)))
        (and
          (or
            (null? left-bst)
            (less-proc (caar left-bst) payload))
          (or
            (null? right-bst)
            (not (less-proc (caar right-bst) payload)))
          (=
            size
            (+
              1
              (if (null? left-bst) 0 (cdar left-bst))
              (if (null? right-bst) 0 (cdar right-bst))))
          (loop left-bst)
          (loop right-bst))))))

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

(define-public (bst-walk less-proc bst-input k)
  (let loop ((bst bst-input))
    (cons
      bst
      (if (null? bst)
        '()
        (let ((payload (caar bst)))
          (if (less-proc k payload)
            (lambda () (loop (cadr bst)))
            (if (less-proc payload k)
              (lambda () (loop (cddr bst)))
              '())))))))

(define-public (equal less-proc a b)
  (and
    (not (less-proc a b))
    (not (less-proc b a))))

(define-public (bst-includes? less-proc bst-input k)
  (let loop ((p (bst-walk less-proc bst-input k)))
    (let ((bst (car p)) (next (cdr p)))
      (if (null? bst)
        #f
        (let ((payload (caar bst)))
          (if (equal less-proc payload k)
            #t
            (if (null? next)
              #f
              (loop (next)))))))))

(define (inc-bsts-seen! bsts-seen)
  (let loop ((bsts bsts-seen))
    (unless (null? bsts)
      (let ((bst (car bsts)))
        (set-cdr! (car bst) (1+ (cdar bst)))
        (loop (cdr bsts))))))

(define (balance-bsts-seen! bsts-seen)
  1)

(define-public (bst-add! less-proc bst-input k)
  (define (bst-size-one)
    (cons (cons k 1) (cons '() '())))

  (if (null? bst-input)
    (bst-size-one)
    (begin
      (let loop ((bst bst-input) (bsts-seen '()))
        (let
          (
            (left-bst (cadr bst))
            (right-bst (cddr bst))
            (payload (caar bst)))
          (if (less-proc k payload)
            (if (null? left-bst)
              (begin
                (inc-bsts-seen! (cons bst bsts-seen))
                (set-car! (cdr bst) (bst-size-one)))
              (loop left-bst (cons bst bsts-seen)))
            (if (null? right-bst)
              (begin
                (inc-bsts-seen! (cons bst bsts-seen))
                (set-cdr! (cdr bst) (bst-size-one)))
              (loop right-bst (cons bst bsts-seen))))))
      bst-input)))

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

(define-public (bst-min-max less-proc bst-input min-max)
  (define proc (if (equal? min-max 'min) cadr cddr))
  (if (null? bst-input)
    '()
    (let loop ((bst bst-input))
      (if (null? (proc bst))
        (caar bst)
        (loop (proc bst))))))

(define-public (bst-min less-proc bst-input)
  (bst-min-max less-proc bst-input 'min))

(define-public (bst-max less-proc bst-input)
  (bst-min-max less-proc bst-input 'max))

(define (leaf? bst)
  (and
    (not (null? bst))
    (null? (cadr bst))
    (null? (cddr bst))))

(define (sub-1-from-path path)
  (let loop ((path path))
    (unless (null? path)
      (let ((bst (car path)))
        (set-cdr! (car bst) (1- (cdar bst)))
        (loop (cdr path))))))

(define-public (bst-delete! less-proc bst-input k-input)
  (let loop ((bst bst-input) (parent '()) (parent-dir '()) (path '()))
    (if (null? bst)
      bst-input
      (let ((payload (caar bst)) (left-bst (cadr bst)) (right-bst (cddr bst)))
        (if (less-proc k-input payload)
          (loop left-bst bst 'left (cons bst path))
          (if (less-proc payload k-input)
            (loop right-bst bst 'right (cons bst path))
            (if (null? left-bst)
              (if (null? right-bst)
                (if (null? parent)
                  '()
                  (begin
                    (if (equal? parent-dir 'left)
                      (set-car! (cdr parent) '())
                      (set-cdr! (cdr parent) '()))
                    (sub-1-from-path path)
                    bst-input))
                (let ((min-right (bst-min less-proc right-bst)))
                  (let
                    (
                      (right-bst-with-deleted
                        (bst-delete! less-proc right-bst min-right)))
                    (set-cdr! (cdr bst) right-bst-with-deleted)
                    (set-car! (car bst) min-right)
                    (sub-1-from-path (cons bst path))
                    bst-input)))
              (let ((max-left (bst-max less-proc left-bst)))
                (let
                  (
                    (left-bst-with-deleted
                      (bst-delete! less-proc left-bst max-left)))
                  (set-car! (cdr bst) left-bst-with-deleted)
                  (set-car! (car bst) max-left)
                  (sub-1-from-path (cons bst path))
                  bst-input)))))))))

(define-public (bst-balance! less-proc bst)
  (if (null? bst)
    '()
    (let ((payload (caar bst)) (left-bst (cadr bst)) (right-bst (cddr bst)))
      (let
        (
          (left-size (if (null? left-bst) 0 (cdar left-bst)))
          (right-size (if (null? right-bst) 0 (cdar right-bst))))
        (let ((diff (- left-size right-size)))
          (if (> diff 1)
            ; take from left
            (let ((max-left (bst-max less-proc left-bst)))
              (set-car! (car bst) max-left)
              (let
                (
                  (left-bst-with-deleted
                    (bst-delete! less-proc left-bst max-left)))
                (bst-add! less-proc bst payload)
                (set-cdr! (car bst) (1- (cdar bst)))
                (bst-balance! less-proc bst)))
            (if (< diff -1)
              ; take from right
              (let ((min-right (bst-min less-proc right-bst)))
                (set-car! (car bst) min-right)
                (let
                  (
                    (right-bst-with-deleted
                      (bst-delete! less-proc right-bst min-right)))
                  (bst-add! less-proc bst payload)
                  (set-cdr! (car bst) (1- (cdar bst)))
                  (bst-balance! less-proc bst)))
              bst)))))))

(define-public (obj-as-string obj)
  (call-with-output-string
    (lambda (port)
      (write obj port))))

(define-public (bst-pretty-print bst-input)
  (define result-port (open-output-string))
  (define display-to-result (lambda (obj) (display obj result-port)))

  (define (r)
    (number->string (random (* 16 16 16 16)) 16))

  (if (null? bst-input)
    (display-to-result '())
    (let loop ((bst bst-input) (bst-id (r)) (parent-index '()))
      (if (null? (car bst))
        (display-to-result '())
        (begin
          (display-to-result bst-id)
          (display-to-result " ")
          (display-to-result parent-index)
          (display-to-result " ")
          (display-to-result (car bst))
          (unless (null? (cadr bst))
            (display-to-result "\n")
            (loop (cadr bst) (r) bst-id))
          (unless (null? (cddr bst))
            (display-to-result "\n")
            (loop (cddr bst) (r) bst-id))))))

  (display (get-output-string result-port))
  (newline))
