(define-module (tests test-db))

(use-modules (srfi srfi-19))

(use-modules (tests util))
(use-modules (bst))
(use-modules (db))

(define-public (test-foo test-fns)
  (define db (create-db))
  (set! db (create-tab db 'player))

  (define record-1 (make-record 'player (cons 'id 1) (cons 'name "foo1")))
  (display-record record-1)

  (define record-2 (make-record 'player (cons 'id 2) (cons 'name "foo2")))
  (display-record record-2)

  (define less-proc (less-proc-for-fields 'player '(id name)))

  (display (less-proc record-1 record-2))(newline)
  (display (less-proc record-2 record-1))(newline)

  ;(let loop ((i 8))
  ;  (when (> i 0)
  ;    (set! db
  ;      (db-insert!
  ;        db 'player (make-record 'player (cons 'name "foo"))))
  ;    (loop (1- i))))
  ;(display-bst (assoc-ref db (list 'table 'player 'index '(id))))(newline)
)

