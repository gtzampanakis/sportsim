(define-module (bst))

(use-modules (srfi srfi-1))

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

(define-public (bst-make)
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

(define-public (bst-add! less-proc bst-input k)
  (define (bst-size-one)
    (cons (cons k 1) (cons '() '())))

  (define balance-all-the-way
    (lambda (bsts-seen)
      (map
        (lambda (bst) (bst-balance! less-proc bst))
        bsts-seen)))

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
                (set-car! (cdr bst) (bst-size-one))
                (balance-all-the-way bsts-seen))
              (loop left-bst (cons bst bsts-seen)))
            (if (null? right-bst)
              (begin
                (inc-bsts-seen! (cons bst bsts-seen))
                (set-cdr! (cdr bst) (bst-size-one))
                (balance-all-the-way bsts-seen))
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

(define-public (bst-min-max less-proc bst-input min-max bound)
  (define proc (if (equal? min-max 'min) cadr cddr))
  (if (null? bst-input)
    '()
    (let loop ((bst bst-input))
      (if (null? (proc bst))
        (caar bst)
        (loop (proc bst))))))

(define-public (bst-min less-proc bst-input)
  (bst-min-max less-proc bst-input 'min '()))

(define-public (bst-max less-proc bst-input)
  (bst-min-max less-proc bst-input 'max '()))

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
                (when (> min-right payload)
                ; If min-right is equal to payload then continuing will lead to
                ; an infinite loop.
                  (set-car! (car bst) min-right)
                  (let
                    (
                      (right-bst-with-deleted
                        (bst-delete! less-proc right-bst min-right)))
                    (bst-add! less-proc bst payload)
                    (set-cdr! (car bst) (1- (cdar bst)))
                    (bst-balance! less-proc bst))))
              bst)))))))

(define-public (obj-as-string obj)
  (call-with-output-string
    (lambda (port)
      (write obj port))))

(define-public (bst-list-print bst-input)
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

(define-public (obj-string-length obj)
  (string-length
    (call-with-output-string
      (lambda (port)
        (display obj port)))))

(define-public (call-n-times n proc)
  (when (> n 0)
    (begin (proc) (call-n-times (1- n) proc))))

(define-public (obj-as-char-list obj)
  (define s
    (let ((result-port (open-output-string)))
      (display obj result-port)
      (get-output-string result-port)))
  (let loop ((r '()) (i (1- (string-length s))))
    (if (= i -1)
      r
      (loop (cons (string-ref s i) r) (1- i)))))

(define-public append-with-min-sizes
  ; Append lists in lss padding each list with spaces if it is shorter than the
  ; respective min-sizes element.
  (lambda (lss min-sizes)
    (apply
      append
      (map
        (lambda (ls min-size)
          (append
            ls
            (let ((s (- min-size (length ls))))
              (if (>= s 0) (make-list s #\space) '()))))
        lss min-sizes))))

(define-public side-by-side-char-matrices
  (lambda char-matrices
    (define min-sizes
      (map char-matrix-width char-matrices))
    (define max-rows (apply max (map length char-matrices)))
    (define char-matrices-with-padding-rows
      (map
        (lambda (char-matrix)
          (let ((d (- max-rows (length char-matrix))))
            (append
              char-matrix
              (make-list d '()))))
        char-matrices))
    (apply
      map
      (append
        (list
          (lambda rows
            (append-with-min-sizes rows min-sizes)))
        char-matrices-with-padding-rows))))

(define-public (display-char-matrix char-matrix)
  (let loop-rows ((i 0))
    (when (< i (length char-matrix))
      (let ((row (list-ref char-matrix i)))
        (let loop-chars ((j 0))
          (when (< j (length row))
            (let ((c (list-ref row j)))
              (display c))
            (loop-chars (1+ j)))))
      (display #\newline)
      (loop-rows (1+ i)))))

(define-public (char-matrix-width char-matrix)
  (if (null? char-matrix)
    0
    (length (car char-matrix))))

(define-public (bst-as-char-matrix bst)
  (if (null? bst)
    (list (obj-as-char-list bst))
    (let*
      (
        (left-bst (cadr bst))
        (right-bst (cddr bst))
        (payload (caar bst))
        (payload-as-char-list (obj-as-char-list payload))
        (left-bst-as-char-matrix (bst-as-char-matrix left-bst))
        (right-bst-as-char-matrix (bst-as-char-matrix right-bst))
        (left-bst-char-matrix-width
          (char-matrix-width left-bst-as-char-matrix))
        (right-bst-char-matrix-width
          (char-matrix-width right-bst-as-char-matrix))
        (both-bsts-as-char-matrix
          (side-by-side-char-matrices
            left-bst-as-char-matrix
            (list (list #\space))
            right-bst-as-char-matrix)))
      (append
        (list
          (append
            payload-as-char-list
            (let
              (
                (d
                  (-
                    (char-matrix-width both-bsts-as-char-matrix)
                    (length payload-as-char-list))))
              (if (> d 0) (make-list d #\space) '())))
          (append
            (list #\|)
            (make-list (1- left-bst-char-matrix-width) #\-)
            (list #\-)
            (list #\+)))
        both-bsts-as-char-matrix))))

(define-public (display-bst bst)
  (display-char-matrix (bst-as-char-matrix bst)))

(define-public (lt less-proc a b)
  (less-proc a b))

(define-public (gte less-proc a b)
  (not (less-proc a b)))

(define-public (gt less-proc a b)
  (and (not (less-proc a b)) (less-proc b a)))

(define-public (eq less-proc a b)
  (and (not (less-proc a b)) (not (less-proc b a))))

(define-public (lte less-proc a b)
  (not (less-proc b a)))

(define-public (bst-for-each less-proc bst proc direction cmp-op cmp-val)
  (define payload-passes?
    (lambda (payload)
      (or
        (null? cmp-op)
        (and (equal? cmp-op 'lt)  (lt  less-proc payload cmp-val))
        (and (equal? cmp-op 'gte) (gte less-proc payload cmp-val)))))
  (let loop ((bst bst))
    (unless (null? bst)
      (let*
        (
          (payload (caar bst))
          (left-bst (cadr bst))
          (right-bst (cddr bst))
          (branches
            (if (less-proc payload cmp-val)
              (cond
                ((equal? cmp-op 'lt) (list 'left 'self 'right))
                ((equal? cmp-op 'gte) (list '() '() 'right)))
              (cond
                ((equal? cmp-op 'lt) (list 'left '() '()))
                ((equal? cmp-op 'gte) (list 'left 'self 'right))))))
        (when (equal? (list-ref branches 0) 'left)
          (loop left-bst))
        (when (equal? (list-ref branches 1) 'self)
          (when (payload-passes? payload)
            (proc payload)))
        (when (equal? (list-ref branches 2) 'right)
          (loop right-bst))))))
