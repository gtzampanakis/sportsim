(use-modules (srfi srfi-19))

(use-modules (lib util))
(use-modules (conf))
(use-modules (tabdef))
(use-modules (bst))
(use-modules (db))

(define (main)
  (display-line "Starting sportsim...")

  (define n-countries (assoc-ref sportsim-conf 'n-countries))
  (define n-teams-per-country (assoc-ref sportsim-conf 'n-teams-per-country))
  (define n-players-per-team (assoc-ref sportsim-conf 'n-players-per-team))
  (define start-date (assoc-ref sportsim-conf 'start-date))
  (define db (create-db))
  (set! db (create-tab db 'country))
  (set! db (create-tab db 'team))
  (set! db (create-tab db 'player))
  (set! db (create-tab db 'competition-type))
  (set! db (create-tab db 'competition))
  (set! db (create-tab db 'event))

  (define (gen-players db team-id country-id)
    (let loop ((i-player 0))
      (when (< i-player n-players-per-team)
        (let ()
          (define r
            (make-record 'player
              (name
                (join
                  "-"
                  "player"
                  (number->string country-id)
                  (number->string team-id)
                  (number->string i-player)))
              (team-id team-id)))
          (set! db (db-insert! db 'player r))
          (loop (1+ i-player)))))
    db)

  (define (gen-teams db country-id)
    (let loop ((i-team 0))
      (when (< i-team n-teams-per-country)
        (let ()
          (define r
            (make-record 'team
              (name
                (join
                  "-" (number->string country-id) (number->string i-team)))
              (country-id country-id)))
          (set! db (db-insert! db 'team r))
          (set! db (gen-players db (record-value r 'id) country-id))
          (loop (1+ i-team)))))
    db)

  (let loop ((i-country 0))
    (when (< i-country n-countries)
      (let ()
        (define r
          (make-record 'country
            (name (join "-" "country" (number->string i-country)))))
        (set! db (db-insert! db 'country r))
        (set! db (gen-teams db (record-value r 'id)))
        (loop (1+ i-country)))))

  (define competition-type-record
    (make-record 'competition-type (name "league")))
  (set! db (db-insert! db 'competition-type competition-type-record))

  (for-each
    (lambda (country-record)
      (define r
        (make-record 'competition
          (start-year (date-year start-date))
          (competition-type-id (record-value competition-type-record 'id))
          (country-id (record-value country-record 'id))))
      (set! db (db-insert! db 'competition r)))
    (query-tab db 'country '() '() '()))

  (for-each
    (lambda (competition-record)
      (define rounds (gen-round-robin n-teams-per-country))
      (define teams
        (query-tab db 'team
          (list
            'country-id 'eq (record-value competition-record 'country-id))
          '() '()))
      (for-each
        (lambda (pair)
          (define event-record
            (make-record 'event
              (datetime 'foo)
              (done? #f)
              (home-team-id 'foo)
              (away-team-id 'foo)
              (competition-id (record-value competition-record 'id))))
          (set! db (db-insert! db 'event event-record)))
        rounds))
    (query-tab db 'competition '() '() '()))


  (display-line "Ending sportsim...")
)

;(use-modules (statprof))
;(statprof main)
(main)

