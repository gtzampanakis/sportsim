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
    (lambda (r) (schedule-league-fixtures-for-country db (vector-ref r 0)))
    (query-tab db 'country (lambda (r) #t))))

(define (schedule-league-fixtures-for-country db country-id)
  (define teams
    (query-tab db 'team
      (lambda (r)
        (equal? (vector-ref r 2) country-id)))) ; fix this hard-coded "2"
  (display (length teams)))

(define (main)
  (set! *random-state* (random-state-from-platform))
  (format #t "Set random seed to ~a\n" (random-state->datum *random-state*))

  (let* (
      (db (create-db))
      (db (create-tab db 'player))
      (db (create-tab db 'country))
      (db (create-tab db 'team))
      (db (create-tab db 'scheduled-item)))

    (let* ((n (assv-ref conf 'n-countries)))
      (create-entities!
        db
        'country
        n
        (lambda (i)
          (make-record country (
            (id i)
            (name (number->string i)))))))

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
            (id i) 
            (name (number->string i))
            (country-id (quotient i ntpc)))))))
    
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
            (id i)
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
