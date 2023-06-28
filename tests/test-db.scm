(define-module (tests test-db))

(use-modules (srfi srfi-19))

(use-modules (tests util))
(use-modules (bst))
(use-modules (db))

(define-public (test-foo test-fns)
  (define db (create-db))
  (set! db (create-tab db 'player))

  (define record-1 (make-record 'player (id 1) (name "foo1")))
  (define record-2 (make-record 'player (id 2) (name "foo2")))
  (define record-3 (make-record 'player (id 3) (name "foo3")))
  (define record-4 (make-record 'player (id 4) (name "foo4")))

  (set! db (db-insert! db 'player record-1))
  (set! db (db-insert! db 'player record-2))
  (set! db (db-insert! db 'player record-3))
  (set! db (db-insert! db 'player record-4))
  (display-bst (assoc-ref db (list 'table 'player 'index '(name))))
)

