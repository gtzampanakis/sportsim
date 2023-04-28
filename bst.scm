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
        (apply bst-size (append (list less-proc) args))))))

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

;(define-public (bst-walk less-proc bst-input k)
;  (if (null? bst-input)
;    (delay '())
;    (let loop ((bst bst-input))
;      (delay (cons bst 

(define-public (bst-add! less-proc bst-input k)
  (define (bst-size-one)
    (cons (cons k 1) (cons '() '())))

  (define (inc-bsts-seen! bsts-seen)
    (let loop ((bsts bsts-seen))
      (unless (null? bsts)
        (let ((bst (car bsts)))
          (set-cdr! (car bst) (1+ (cdar bst)))
          (loop (cdr bsts))))))

  (if (null? bst-input)
    (bst-size-one)
    (begin
      (let loop ((bst bst-input) (bsts-seen '()))
        (let
          (
            (left-bst (cadr bst))
            (right-bst (cddr bst))
            (payload (caar bst))
            (size (cdar bst)))
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

(define-public (bst-includes? less-proc bst-input k)
  (not (null? (bst-member less-proc bst-input k))))

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

;(define-public (bst-delete! less-proc bst-input k-input)
;  (unless (null? (car bst-input))
;    (let loop ((bst bst-input) (parent-bst '()) (dir-from-parent '()))
;      (let ((k (caar bst)) (left-bst (cadr bst)) (right-bst (cddr bst)))
;        (if (less-proc k-input k)
;          ; check left
;          (unless (null? left-bst)
;            (loop left-bst bst 'left))
;          (if (less-proc k k-input)
;            ; check right
;            (unless (null? right-bst)
;              (loop right-bst bst 'right))
;            ; equality
;            (if (null? parent-bst)
;              (set-car! bst '())
;              (if (equal? dir-from-parent 'left)
;                (if (leaf? bst)
;                  ; delete altogether
;                  (set-car! (cdr parent-bst) '())


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
