(define-module (tabdef))

(define-public db-meta
  (lambda args
    (cond

      ((equal? args '(fields player))
        '(id name dob team-id))
      ((equal? args '(field-types player))
        (list
          'number
          'string
          'string
          'number))
      ((equal? args '(indices player))
        (list
          '(id)
          '(name)
          '(dob)
          '(team-id)))

      ((equal? args '(fields country))
        '(id name))
      ((equal? args '(field-types country))
        '(number string))
      ((equal? args '(indices country))
        (list
          '(id)
          '(name)))

      ((equal? args '(fields team))
        '(id name country-id))
      ((equal? args '(field-types team))
        '(number string number))
      ((equal? args '(indices team))
        (list
          '(id)
          '(name)))

      ((equal? args '(fields competition-series))
        '(id name country-id))
      ((equal? args '(field-types competition-series))
        '(number string number))
      ((equal? args '(indices competition-series))
        (list
          '(id)
          '(name)))

      ((equal? args '(fields competition))
        '(id name start-year country-id))
      ((equal? args '(field-types competition))
        '(number string string number))
      ((equal? args '(indices competition))
        (list
          '(id)
          '(name)))

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
        '(id year month day proc)))

))
