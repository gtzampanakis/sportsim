(define-module (db)
  #:export (
    make-record
    field-to-index
    fields-to-values
    record-attr
    record-set-attr!
    query-tab))

(use-modules (srfi srfi-19))

(use-modules (lib util))
(use-modules (tabdef))

(define-public (create-db)
  '())

(define max-id (expt 2 64))

(define-public (generate-id)
  (number->string (random max-id) 16))

(define-public (create-tab db tab-name)
  (let ((pair (cons (list 'table tab-name 'data) (make-hash-table))))
    (cons pair db)))

;(define-public (create-index db tab-name field-names)
;  )

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
        ((string>? v1 v2) 1)))
    ((and (date? v1) (date? v2))
      (compare-dates v1 v2))))

(define-public (less-records r1 r2 field-indices)
  (if (null? field-indices)
    #f
    (let ((field-index (car field-indices)))
      (define v1 (vector-ref r1 field-index))
      (define v2 (vector-ref r2 field-index))
      (define compare-result (compare-values v1 v2))
      (cond
        ((= compare-result 0)
          (less-records r1 r2 (cdr field-indices)))
        ((< compare-result 0)
          #t)
        (else
          #f)))))

(define-public (find-index obj ls)
  (let loop ((e 0) (ls ls))
    (if (null? ls)
      #f
      (if (equal? obj (car ls))
        e
        (loop (1+ e) (cdr ls))))))

(define-public (find-indices objs ls)
  (map
    (lambda (obj) (find-index obj ls))
    objs))

(define*
    (query-tab
      db tab-name #:key (pred '()) (order-by '()) (limit -1))
  (let (
      (tab (cdr (assoc (list 'table tab-name 'data) db)))
      (records '())
      (n 0)
      (field-indices (find-indices order-by (db-meta 'fields tab-name))))
    (hash-for-each-handle
      (lambda (handle)
        (let ((k (car handle)) (v (cdr handle)))
          (if (or (null? pred) (pred v))
            (begin
              (set! records
                (merge
                  (list v)
                  records
                  (lambda (r1 r2)
                    (less-records r1 r2 field-indices))))
              (set! n (1+ n))))))
      tab)
    (cond
      ((null? records) records)
      ((= limit -1) records)
      (else (list-head records limit)))))

(define-public (query-tab-by-id db tab-name id)
  (define tab (cdr (assoc (list 'table tab-name 'data) db)))
  (hash-ref tab id))

(define-syntax field-to-index
  (lambda (x)
    (syntax-case x ()
      ((_ field-to-find (field ...))
        #'(field-to-index field-to-find (field ...) 0))
      ((_ field-to-find () i)
        #''())
      ((_ field-to-find (field1 field2 ...) i)
        (if (equal? #'field-to-find #'field1)
          #'i
          (let ((i (syntax->datum #'i)))
            #`(field-to-index field-to-find (field2 ...) #,(+ 1 i))))))))

(define-syntax field-to-value
  (lambda (x)
    (syntax-case x ()
      ((_ field ())
        #''())
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

(define-syntax record-set-attr!
  (lambda (x)
    (syntax-case x ()
      ((_ tab field record obj)
        (let (
            (fields
              (datum->syntax #'tab (db-meta 'fields (syntax->datum #'tab)))))
          #`(vector-set! record (field-to-index field #,fields) obj))))))
