(define (create-db)
  '())

(define (create-tab db tab-name)
  (let ((pair (cons (list 'table tab-name 'data) (make-hash-table))))
    (cons pair db)))

(define (insert-record! db tab-name record)
  (let ((h (cdr (assoc (list 'table tab-name 'data) db))))
    (hash-set! h (vector-ref record 0) record)))

(define query-tab
  (case-lambda
    ((db tab-name pred) (query-tab db tab-name pred -1))
    ((db tab-name pred limit)
      (if (= limit 0) '()
        (let (
            (tab (cdr (assoc (list 'table tab-name 'data) db)))
            (records '())
            (n 0)
            (should-check-limit (> limit 0)))
          (call-with-prompt
            'r
            (lambda ()
              (hash-for-each-handle
                (lambda (handle)
                  (let ((k (car handle)) (v (cdr handle)))
                    (if (pred v)
                      (begin
                        (set! records (cons v records))
                        (set! n (+ n 1))
                        (when (and should-check-limit (>= n limit))
                          (abort-to-prompt 'r))))))
                tab)
              records)
            (lambda (_) records)))))))

(define (query-tab-single db tab-name pred)
  (define results (query-tab db tab-name pred 1))
  (if (null? results) '() (car results)))

(define-syntax make-record
  (syntax-rules ()
    ((_ tab-name ((field-name val) ...))
      (let ((vect (make-vector (db-meta tab-name nf))))
        (vector-set! vect (db-meta tab-name fi field-name) val) ...
        vect))))
