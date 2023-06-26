(define-module (tests test-db))

(use-modules (srfi srfi-19))

(use-modules (tests util))
(use-modules (bst))
(use-modules (db))

(define-public (test-foo test-fns)
  (define db (create-db))
  (set! db (create-tab db 'player))
  (display (field-type 'player 'id))(newline)
  (display (field-index 'player 'id))(newline)
  (define record (make-record 'player (cons 'name "foo")))
  (display record)(newline)
  (display (record-value record 'dob))(newline)
  ;(let loop ((i 8))
  ;  (when (> i 0)
  ;    (set! db
  ;      (db-insert!
  ;        db 'player (make-record 'player (cons 'name "foo"))))
  ;    (loop (1- i))))
  ;(display-bst (assoc-ref db (list 'table 'player 'index '(id))))(newline)
)

