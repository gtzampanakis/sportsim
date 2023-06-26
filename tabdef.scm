(define-module (tabdef))

(define-public db-meta
  (lambda args
    (cond
      ((equal? args '(fields player))
        '(id name dob team-id))
      ((equal? args '(field-types player))
        '(integer string string integer))
      ((equal? args '(indices player))
        (list
          '(id)
          '(name)
          '(dob)))
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
