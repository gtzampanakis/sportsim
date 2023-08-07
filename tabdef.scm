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
          '(name)
          '(country-id)))

      ((equal? args '(fields competition-type))
        '(id name))
      ((equal? args '(field-types competition-type))
        '(number string))
      ((equal? args '(indices competition-type))
        (list
          '(id)
          '(name)))

      ((equal? args '(fields competition))
        '(
          id
          start-year
          competition-type-id
          country-id))
      ((equal? args '(field-types competition))
        '(
          number
          number
          number
          number))
      ((equal? args '(indices competition))
        (list
          '(id)))

      ((equal? args '(fields event))
        '(
          id
          datetime
          done?
          home-team-id
          away-team-id
          competition-id))
      ((equal? args '(field-types event))
        '(
          number
          string
          number
          number
          number
          number))
      ((equal? args '(indices event))
        (list
          '(id)
          '(datetime)))

)))
