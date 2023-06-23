(define-module (tests test-db))

(use-modules (srfi srfi-19))

(use-modules (tests util))
(use-modules (bst))
(use-modules (db))

(define-public (test-foo test-fns)
  (define db (create-db))
  (set! db (create-tab db 'player))
  (let loop ((i 8))
    (when (> i 0)
      (set! db
        (db-insert!
          db 'player (make-record 'player (cons 'name "foo"))))
      (loop (1- i))))
  (display-bst (assoc-ref db (list 'table 'player 'data)))(newline)
)

