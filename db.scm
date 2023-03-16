(define-module (db)
  #:export (make-record field-to-index fields-to-values record-attr))

(use-modules (tabdef))

(define-public (create-db)
  '())

(define max-id (expt 2 64))

(define-public (generate-id)
  (number->string (random max-id) 16))

(define-public (create-tab db tab-name)
  (let ((pair (cons (list 'table tab-name 'data) (make-hash-table))))
    (cons pair db)))

(define-public (insert-record! db tab-name record)
  (let ((h (cdr (assoc (list 'table tab-name 'data) db))))
    (hash-set! h (vector-ref record 0) record)))

(define-public (compare-values v1 v2)
  (cond
    ((and (number? v1) (number? v2))
      (- v1 v2))
    ((and (string? v1) (string? v2))
      (cond
        ((string=? v1 v2) 0)
        ((string<? v1 v2) -1)
        ((string>? v1 v2) 1)))))

(define-public (compare-records r1 r2 field-indices)
  (if (null? field-indices)
    #f
    (let ((field-index (car field-indices)))
      (define v1 (vector-ref r1 field-index))
      (define v2 (vector-ref r2 field-index))
      (define compare-result (compare-values v1 v2))
      (cond
        ((= compare-result 0)
          (compare-records r1 r2 (cdr field-indices)))
        ((< compare-result 0)
          #t)
        (else
          #f)))))

(define-public query-tab
  (case-lambda
    ((db tab-name) (query-tab db tab-name (lambda (r) #t)))
    ((db tab-name pred) (query-tab db tab-name pred '()))
    ((db tab-name pred order-by) (query-tab db tab-name pred order-by -1))
    ((db tab-name pred order-by limit)
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
                        ;(set! records
                        ;  (merge
                        ;    (list v)
                        ;    records
                        ;    compare))
                        (set! n (1+ n))
                        (when (and should-check-limit (>= n limit))
                          (abort-to-prompt 'r))))))
                tab)
              records)
            (lambda (_) records)))))))

(define-public (query-tab-single db tab-name pred)
  (define results (query-tab db tab-name pred 1))
  (if (null? results) '() (car results)))

(define-syntax field-to-index
  (lambda (x)
    (syntax-case x ()
      ((_ field-to-find (field ...))
        #'(field-to-index field-to-find (field ...) 0))
      ((_ field-to-find () i)
        #''nil)
      ((_ field-to-find (field1 field2 ...) i)
        (if (equal? #'field-to-find #'field1)
          #'i
          (let ((i (syntax->datum #'i)))
            #`(field-to-index field-to-find (field2 ...) #,(+ 1 i))))))))

(define-syntax field-to-value
  (lambda (x)
    (syntax-case x ()
      ((_ field ())
        #''nil)
      ((_ field ((k1 v1) . tail))
        (if (equal? (syntax->datum #'field) (syntax->datum #'k1))
          #'v1
          #'(field-to-value field tail))))))

(define-syntax fields-to-values
  (lambda (x)
    (syntax-case x ()
      ((_ (field1 ...) assignments)
        #'(list (field-to-value field1 assignments) ...)))))

(define-syntax make-record
  (lambda (x)
    (syntax-case x ()
      ((_ tab assignments)
        (let (
            (fields
              (datum->syntax #'tab (db-meta 'fields (syntax->datum #'tab))))
            (assignments
              (datum->syntax #'tab
                (append
                  (syntax->datum #'assignments) '((id (generate-id)))))))
          #`(apply vector (fields-to-values #,fields #,assignments)))))))

(define-syntax record-attr
  (lambda (x)
    (syntax-case x ()
      ((_ tab field record)
        (let (
            (fields
              (datum->syntax #'tab (db-meta 'fields (syntax->datum #'tab)))))
          #`(vector-ref record (field-to-index field #,fields)))))))
