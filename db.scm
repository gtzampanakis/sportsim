(define (create-db)
  '())

(define (create-tab db tab-name)
  (let ((pair (cons (list 'table tab-name 'data) (make-hash-table))))
    (cons pair db)))

(define (insert-record! db tab-name record)
  (let ((h (cdr (assoc (list 'table tab-name 'data) db))))
    (hashq-set! h (vector-ref record 0) record)))
