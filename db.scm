(define (create-db)
  '())

(define (create-tab db tab-name)
  (let ((pair (cons (list 'table tab-name 'data) (make-hash-table))))
    (cons pair db)))

(define (insert-record! db tab-name record)
  (let ((h (cdr (assoc (list 'table tab-name 'data) db))))
    (hash-set! h (vector-ref record 0) record)))

(define (query-tab db tab-name pred limit)
  (define tab (cdr (assoc (list 'table tab-name 'data) db)))
  (define result '())
  (define n 0)
  (call/cc
    (lambda (cont)
      (hash-for-each-handle
        (lambda (handle)
          (let ((k (car handle)) (v (cdr handle)))
            (if (pred v)
              (begin
                (set! result (cons v result))
                (set! n (+ n 1))
                (when (>= n limit)
                  (cont result))))))
        tab)
      result)))

(define (query-tab-single db tab-name pred)
  (define results (query-tab db tab-name pred 1))
  (if (null? results) '() (car results)))
