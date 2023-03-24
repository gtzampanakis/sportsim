(define-module (conf))

(use-modules (srfi srfi-19))

(use-modules (lib util))

(define-public conf (list
  (cons 'start-date (date 2022 8 1))
  (cons 'stop-date (date 2032 8 1))
  (cons 'n-countries 1)
  (cons 'n-players-per-team 11)
  (cons 'n-teams-per-country 18)))
