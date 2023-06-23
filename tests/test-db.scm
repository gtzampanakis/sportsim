(define-module (tests test-db))

(use-modules (srfi srfi-19))

(use-modules (tests util))
(use-modules (bst))
(use-modules (db))

(define-public (test-foo test-fns)
  (define db (create-db))
  (set! db (create-tab db 't))
  (set! db (insert-record! db 't (list 0 "a")))
  (set! db (insert-record! db 't (list 1 "a")))
  (set! db (insert-record! db 't (list 2 "a")))
  (set! db (insert-record! db 't (list 3 "a")))
  (set! db (insert-record! db 't (list 4 "a")))
  (set! db (insert-record! db 't (list 5 "a")))
  (set! db (insert-record! db 't (list 6 "a")))
  (set! db (insert-record! db 't (list 7 "a")))
  (display-bst (assoc-ref db (list 'table 't 'data)))(newline)
)

