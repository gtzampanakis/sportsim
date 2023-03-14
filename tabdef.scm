(define-module (tabdef)
  #:export (db-meta))

(eval-when (expand load eval)
  (define db-meta
    (lambda args
      (cond
        ((equal? args '(fields player))
          '(id name dob team-id ratings))
        ((equal? args '(fields country))
          '(id name))
        ((equal? args '(fields team))
          '(id name country-id))
        ((equal? args '(fields scheduled-item))
          '(id datetime team-home team-away))))))
