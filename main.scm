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

(define (schedule-league-fixtures db)
  (define countries (cdr (assoc (list 'table 'country 'data) db)))
  (for-each
    (lambda (r)
      (schedule-league-fixtures-for-country db (record-attr country id r)))
    (query-tab db 'country)))

(define (schedule-league-fixtures-for-country db country-id)
  (define teams
    (query-tab db 'team
      (lambda (r)
        (equal? (record-attr team country-id r) country-id))))
  (define n-teams (length teams))
  (define matches-per-round (quotient n-teams 2))

  (assert (= (remainder n-teams 2) 0)))

(define (main)
  (set! *random-state* (random-state-from-platform))
  (format #t "Set random seed to ~a\n" (random-state->datum *random-state*))

  (let* (
      (db (create-db))
      (db (create-tab db 'player))
      (db (create-tab db 'country))
      (db (create-tab db 'team))
      (db (create-tab db 'scheduled-item)))

    ; Generate countries.
    (let* ((n (assv-ref conf 'n-countries)))
      (create-entities!
        db
        'country
        n
        (lambda (i)
          (make-record country (
            (name (number->string i)))))))

    ; Generate teams and assign them to countries.
    (let* (
        (ntpc (assv-ref conf 'n-teams-per-country))
        (nc (assv-ref conf 'n-countries))
        (n (* ntpc nc)))
      (create-entities!
        db
        'team
        n
        (lambda (i)
          (make-record team (
            (name (number->string i))
            (country-id (quotient i ntpc)))))))

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
            (name (number->string i))
            (dob (date 2004 1 1))
            (team-id (quotient i nppt))
            (ratings #(50 50)))))))

    (let (
        (start-date (assv-ref conf 'start-date))
        (stop-date (assv-ref conf 'stop-date)))
      (let loop ((current-date start-date))
        (when (< (date->ts current-date) (date->ts stop-date))
          (format #t "Current date: ~a\n" current-date)
          (let (
              (month (date-month current-date))
              (day (date-day current-date)))
            (when
                (and
                  (= month (date-month start-date))
                  (= day (date-day start-date)))
              (schedule-league-fixtures db))
          (loop (add-day current-date))))))

))
      

    ; Make a schedule data structure and fill it up periodically.

(main)
