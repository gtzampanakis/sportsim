(define-module (tests test-db))

(use-modules (srfi srfi-19))

(use-modules (tests util))
(use-modules (bst))
(use-modules (db))

(define-public (test-foo test-fns)
  (define db (create-db))
  (set! db (create-tab db 'player))

  (define record-1 (make-record 'player (cons 'id 1) (cons 'name "foo1")))
  (define record-2 (make-record 'player (cons 'id 2) (cons 'name "foo2")))
  (define record-3 (make-record 'player (cons 'id 3) (cons 'name "foo3")))
  (define record-4 (make-record 'player (cons 'id 4) (cons 'name "foo4")))

  (define less-proc (less-proc-for-fields 'player '(id name)))

  (set! db (db-insert! db 'player record-1))
  (set! db (db-insert! db 'player record-2))
  (set! db (db-insert! db 'player record-3))
  (set! db (db-insert! db 'player record-4))
  (display-bst (assoc-ref db (list 'table 'player 'index '(dob))))

  ;(let loop ((i 8))
  ;  (when (> i 0)
  ;    (set! db
  ;      (db-insert!
  ;        db 'player (make-record 'player (cons 'name "foo"))))
  ;    (loop (1- i))))
  ;(display-bst (assoc-ref db (list 'table 'player 'index '(id))))(newline)
)

