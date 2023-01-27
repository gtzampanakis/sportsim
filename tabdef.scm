(define-syntax db-meta
  (syntax-rules (
    fi nf
    player country
    id name
  )
    ((_ player nf) 5)
    ((_ player fi id) 0)
    ((_ player fi name) 1)
    ((_ player fi dob) 2)
    ((_ player fi team-id) 3)
    ((_ player fi ratings) 4)

    ((_ country nf) 2)
    ((_ country fi id) 0)
    ((_ country fi name) 1)

    ((_ team nf) 3)
    ((_ team fi id) 0)
    ((_ team fi name) 1)
    ((_ team fi country-id) 2)

    ((_ scheduled-item nf) 3)
    ((_ scheduled-item fi id) 0)
    ((_ scheduled-item fi name) 1)
    ((_ scheduled-item fi date) 2)))
