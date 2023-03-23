(import (rnrs base (6)))

(use-modules (srfi srfi-9))
(use-modules (srfi srfi-19))
(use-modules (ice-9 format))

(use-modules (lib util))
(use-modules (conf))
(use-modules (tabdef))
(use-modules (db))

(define (create-entities! db tab-name n creator-proc)
  (let loop ((i 0))
    (when (< i n)
      (let ((r (creator-proc i)))
        (insert-record! db tab-name r)
        (loop (+ i 1))))))

(define (schedule-league-fixtures db current-date)
  (define countries (cdr (assoc (list 'table 'country 'data) db)))
  (for-each
    (lambda (r)
      (schedule-league-fixtures-for-country
        db
        (record-attr country id r)
        current-date))
    (query-tab db 'country #:order-by '(name))))

(define (schedule-league-fixtures-for-country db country-id current-date)
  (define n-runs 2)
  (define teams
    (list->vector
      (query-tab db 'team
        #:pred
          (lambda (r)
            (equal? (record-attr team country-id r) country-id))
        #:order-by '(name))))
  (define n-teams (vector-length teams))
  (define rounds (gen-round-robin n-teams))
  (define round-date
    (let loop ((round-date current-date))
      (if (= (date->dow round-date) 6)
        round-date
        (loop (add-day round-date)))))
  (define competition
    (make-record competition (
      (name
        (string-append
          "league-"
          (number->string (date-year current-date))
          "-"
          country-id))
      (country-id country-id)
      (start-year (date-year current-date)))))
  (let loop-over-runs ((round-date round-date) (i-run 0))
    (unless (= i-run n-runs)
      (let loop-over-rounds ((round-date round-date) (rounds rounds))
        (unless (null? rounds)
          (let loop-over-pairs ((pairs (car rounds)))
            (unless (null? pairs)
              (let* (
                  (pair
                    (
                      (if (= (remainder i-run 2) 0)
                        identity
                        get-switched-pair)
                      (car pairs)))
                  (team-home-index (car pair))
                  (team-away-index (cdr pair))
                  (team-home (vector-ref teams team-home-index))
                  (team-away (vector-ref teams team-away-index))
                  (record
                    (make-record event (
                      (year (date-year round-date))
                      (month (date-month round-date))
                      (day (date-day round-date))
                      (done? #f)
                      (team-home team-home)
                      (team-away team-away)
                      (competition-id
                        (record-attr competition id competition))))))
                (display record)(newline)
                (insert-record!
                  db
                  'event
                  record)
                (loop-over-pairs (cdr pairs)))))
        (loop-over-rounds (add-days round-date 7) (cdr rounds)))
      (loop-over-runs round-date (1+ i-run))))))

(define (schedule-next-event db scheduled-item-record as-of-date)
  (define next-date
    (next-date-for-schedule as-of-date
      (record-attr scheduled-item year scheduled-item-record)
      (record-attr scheduled-item month scheduled-item-record)
      (record-attr scheduled-item day scheduled-item-record)))
  (when next-date
    (insert-record!
      db 'event
      (make-record event (
        (datetime next-date)
        (done? #f)
        (proc (record-attr scheduled-item proc scheduled-item-record))
        (scheduled-item-id
          (record-attr scheduled-item id scheduled-item-record)))))))

(define (process-event event-record)
  (define proc (record-attr event proc event-record))
  ;(proc event-record)
  (display proc)(newline)
  (record-set-attr! event done? event-record #t))

(define (main)
  (set! *random-state* (random-state-from-platform))
  (format #t "Set random seed to ~a\n" (random-state->datum *random-state*))

  (define db
    (create-tab
    (create-tab
    (create-tab
    (create-tab
    (create-tab
    (create-db)
    'player)
    'country)
    'team)
    'event)
    'scheduled-item))

  (define start-date (assv-ref conf 'start-date))
  (define stop-date (assv-ref conf 'stop-date))

  ; Generate countries.
  (let* ((n (assv-ref conf 'n-countries)))
    (create-entities!
      db
      'country
      n
      (lambda (i)
        (make-record country (
          (name (string-append "country-" (number->string i))))))))

  ; Generate teams and assign them to countries.
  (let* (
      (ntpc (assv-ref conf 'n-teams-per-country))
      (nc (assv-ref conf 'n-countries))
      (n (* ntpc nc))
      (countries
        (query-tab db 'country #:order-by '(id))))
    (create-entities!
      db
      'team
      n
      (lambda (i)
        (make-record team (
          (name (string-append "team-" (number->string i)))
          (country-id
            (record-attr
              country
              id
              (list-ref countries (remainder i nc)))))))))

  ; Generate players and assign them to teams.
  (let* (
      (nppt (assv-ref conf 'n-players-per-team))
      (ntpc (assv-ref conf 'n-teams-per-country))
      (nc (assv-ref conf 'n-countries))
      (n (* nc ntpc nppt)))
    (create-entities!
      db
      'player
      n
      (lambda (i)
        (make-record player (
          (name (string-append "player-" (number->string i)))
          (dob (date 2004 1 1))
          (team-id (quotient i nppt))
          (ratings #(50 50)))))))

  ;(let (
  ;    (start-date (assv-ref conf 'start-date))
  ;    (stop-date (assv-ref conf 'stop-date)))
  ;  (let loop ((current-date start-date))
  ;    (when (< (date->ts current-date) (date->ts stop-date))
  ;      (format #t "Current date: ~a\n" current-date)
  ;      (let (
  ;          (month (date-month current-date))
  ;          (day (date-day current-date)))
  ;        (when
  ;            (and
  ;              (= month (date-month start-date))
  ;              (= day (date-day start-date)))
  ;          (schedule-league-fixtures db current-date))
  ;      (loop (add-day current-date))))))

  (insert-record!
    db 'scheduled-item
    (make-record scheduled-item (
      (year (date-year stop-date))
      (month (date-month stop-date))
      (day (date-day stop-date))
      (proc exit))))
  
  (insert-record!
    db 'scheduled-item
    (make-record scheduled-item (
      (month (date-month start-date))
      (day (date-day start-date))
      (proc
        (lambda (event-record)
          (schedule-league-fixtures
            db (record-attr event datetime event-record)))))))

  ;(display (query-tab db 'scheduled-item))(newline)

  (for-each
    (lambda (r) (schedule-next-event db r start-date))
    (query-tab db
      'scheduled-item
      #:order-by '(id)))

  ;(display-list
  ;  (query-tab db 'event #:order-by '(datetime)))

  (let loop ((current-date start-date))
    (let (
        (next-events
          (query-tab db 'event
            #:pred
              (lambda (e)
                (equal? (record-attr event done? e) #f))
            #:order-by
              '(datetime)
            #:limit 100)))
      (if (null? next-events)
        #f
        (let ()
          (for-each
            (lambda (e)
              (process-event e))
            next-events)
          (loop current-date)))))

)

    ; Make a schedule data structure and fill it up periodically.

(main)
