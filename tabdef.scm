(define-module (tabdef)
  #:export (db-meta))

(define db-meta
  (lambda args
    (cond
      ((equal? args '(fields player))
        '(id name dob team-id ratings))
      ((equal? args '(fields country))
        '(id name))
      ((equal? args '(fields team))
        '(id name country-id))
      ((equal? args '(fields competition))
        '(id name start-year country-id))
      ((equal? args '(fields event))
        '(
          id
          datetime
          proc
          done?
          team-home
          team-away
          scheduled-item-id
          competition-id))
      ((equal? args '(fields scheduled-item))
        '(id year month day proc)))))
