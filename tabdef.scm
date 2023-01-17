(define tabinfo (list
  (cons 'rating (list
    (cons 'fields '(att def))))
  (cons 'player (list
    (cons 'fields '(id name dob team-id ratings))))
  (cons 'country (list
    (cons 'fields '(id name))))
  (cons 'team (list
    (cons 'fields '(id name country-id))))))

(define-syntax fi
  (lambda (x)
    (syntax-case x (player country id name dob)
      ((_ player id) #'0)
      ((_ player name) #'1)
      ((_ player dob) #'2)
      ((_ player team-id) #'3)
      ((_ player ratings) #'4)
      ((_ country id) #'0)
      ((_ country name) #'1)
      ((_ team id) #'0)
      ((_ team name) #'1)
      ((_ team country-id) #'2))))
