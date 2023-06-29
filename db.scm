(define-module (db)
  #:export (make-record))

(use-modules (srfi srfi-19))

(use-modules (lib util))
(use-modules (bst))
(use-modules (tabdef))

(define max-id (expt 2 64))
(define less-proc <)

(define-public (create-db) (bst-make))

(define-public (generate-id)
  (random max-id))

(define-public (create-tab db tab-name)
  (define indices (db-meta 'indices tab-name))
  (for-each
    (lambda (fields)
      (set! db
        (cons
          (cons (list 'table tab-name 'index fields) (bst-make))
          db)))
    indices)
  db)

(define-public field-type
  (memoized-proc
    (lambda (tab-name field)
      (call-with-prompt
        'esc
        (lambda ()
          (for-each
            (lambda (tabdef-field-name tabdef-field-type)
              (if (equal? tabdef-field-name field)
                (abort-to-prompt 'esc tabdef-field-type)))
            (db-meta 'fields tab-name)
            (db-meta 'field-types tab-name)))
        (lambda (cont return-value) return-value)))))

(define-public (field-index tab-name field)
  (let loop ((i 0) (fields (db-meta 'fields tab-name)))
    (if (equal? (car fields) field)
      i
      (loop (1+ i) (cdr fields)))))

(define-public (record-value record field)
  (define tab-name (assoc-ref record 'tab-name))
  (define record-data (assoc-ref record 'data))
  (vector-ref record-data (field-index tab-name field)))

(define (less-proc-respecting-nulls less-proc)
  (lambda (a b)
    (cond
      ((and (null? b) (not (null? a))) #t)
      ((and (null? a) (not (null? b))) #f)
      ((and (null? a) (null? b)) #f)
      (else (less-proc a b)))))

(define-public (less-proc-for-field tab-name field)
  (define ft (field-type tab-name field))
  (less-proc-respecting-nulls
    (cond
      ((equal? ft 'number) <)
      ((equal? ft 'string) string<?))))

(define-public (display-record record)
  (define tab-name (assoc-ref record 'tab-name))
  (define record-data (assoc-ref record 'data))
  (display "Record ")(display tab-name)(newline)
  (for-each
    (lambda (field value)
      (display field)(display ": ")(display value)(newline))
    (db-meta 'fields tab-name)
    (vector->list record-data))
  (newline))

(define-public less-proc-for-fields
  (lambda (tab-name fields)
    (lambda (record-1 record-2)
      (call-with-prompt
        'esc
        (lambda ()
          (for-each
            (lambda (field)
              (define less-proc (less-proc-for-field tab-name field))
              (define value-1 (record-value record-1 field))
              (define value-2 (record-value record-2 field))
              (if (less-proc value-1 value-2)
                (abort-to-prompt 'esc #t)
                (if (less-proc value-2 value-1)
                  (abort-to-prompt 'esc #f)
                  '())))
            fields)
          (abort-to-prompt 'esc #f))
        (lambda (cont return-value) return-value)))))

(define-public -make-record
  (lambda (tab-name . pairs)
    (define field-names (db-meta 'fields tab-name))
    (define data-vector
      (list->vector
        (map
          (lambda (field-name)
            (call-with-prompt
              'esc
              (lambda ()
                (for-each
                  (lambda (pair)
                    (let ((k (car pair)) (v (cdr pair)))
                      (when (equal? field-name k)
                        (abort-to-prompt 'esc v))))
                  pairs)
                (if (equal? field-name 'id) (generate-id) '()))
              (lambda (cont return-value) return-value)))
          field-names)))
    (list
      (cons 'tab-name tab-name)
      (cons 'data data-vector))))

(define-public (db-insert! db tab-name record)
  (define indices (db-meta 'indices tab-name))
  (for-each
    (lambda (fields)
      (define index (assoc-ref db (list 'table tab-name 'index fields)))
      (define less-proc (less-proc-for-fields tab-name fields))
      (define new-index
        (bst-add! less-proc index
          (cons
            (map
              (lambda (field)
                (record-value record field))
            fields)
            record)))
      (assoc-set! db (list 'table tab-name 'index fields) new-index))
    indices)
  db)

(define-syntax make-record
  (syntax-rules ()
    ((_ tab-name)
      (-make-record tab-name))
    ((_ tab-name (field1 value1) ...)
      (-make-record tab-name (cons (quote field1) value1) ...))))

(define-public (query-tab db tab-name pred order-by limit)
  1)

;(define*
;    (query-tab
;      db tab-name #:key (pred '()) (order-by '()) (limit -1))
;  (let (
;      (tab (cdr (assoc (list 'table tab-name 'data) db)))
;      (records '())
;      (n 0)
;      (field-indices (find-indices order-by (db-meta 'fields tab-name))))
;    (hash-for-each-handle
;      (lambda (handle)
;        (let ((k (car handle)) (v (cdr handle)))
;          (if (or (null? pred) (pred v))
;            (begin
;              (set! records
;                (merge
;                  (list v)
;                  records
;                  (lambda (r1 r2)
;                    (less-records r1 r2 field-indices))))
;              (set! n (1+ n))))))
;      tab)
;    (cond
;      ((null? records) records)
;      ((= limit -1) records)
;      (else (list-head records limit)))))
;
;(define-public (query-tab-by-id db tab-name id)
;  (define tab (cdr (assoc (list 'table tab-name 'data) db)))
;  (hash-ref tab id))
;
;(define-syntax field-to-index
;  (lambda (x)
;    (syntax-case x ()
;      ((_ field-to-find (field ...))
;        #'(field-to-index field-to-find (field ...) 0))
;      ((_ field-to-find () i)
;        #''())
;      ((_ field-to-find (field1 field2 ...) i)
;        (if (equal? #'field-to-find #'field1)
;          #'i
;          (let ((i (syntax->datum #'i)))
;            #`(field-to-index field-to-find (field2 ...) #,(+ 1 i))))))))
;
;(define-syntax field-to-value
;  (lambda (x)
;    (syntax-case x ()
;      ((_ field ())
;        #''())
;      ((_ field ((k1 v1) . tail))
;        (if (equal? (syntax->datum #'field) (syntax->datum #'k1))
;          #'v1
;          #'(field-to-value field tail))))))
;
;(define-syntax fields-to-values
;  (lambda (x)
;    (syntax-case x ()
;      ((_ (field1 ...) assignments)
;        #'(list (field-to-value field1 assignments) ...)))))
;
;(define-syntax make-record
;  (lambda (x)
;    (syntax-case x ()
;      ((_ tab assignments)
;        (let (
;            (fields
;              (datum->syntax #'tab (db-meta 'fields (syntax->datum #'tab))))
;            (assignments
;              (datum->syntax #'tab
;                (append
;                  (syntax->datum #'assignments) '((id (generate-id)))))))
;          #`(apply vector (fields-to-values #,fields #,assignments)))))))
;
;(define-syntax record-attr
;  (lambda (x)
;    (syntax-case x ()
;      ((_ tab field record)
;        (let (
;            (fields
;              (datum->syntax #'tab (db-meta 'fields (syntax->datum #'tab)))))
;          #`(vector-ref record (field-to-index field #,fields)))))))
;
;(define-syntax record-set-attr!
;  (lambda (x)
;    (syntax-case x ()
;      ((_ tab field record obj)
;        (let (
;            (fields
;              (datum->syntax #'tab (db-meta 'fields (syntax->datum #'tab)))))
;          #`(vector-set! record (field-to-index field #,fields) obj))))))
