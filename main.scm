; (import (rnrs base (6)))
; 
; (use-modules (srfi srfi-9))
; (use-modules (srfi srfi-19))
; (use-modules (ice-9 format))
; (use-modules (ice-9 time))

(use-modules (lib util))
(use-modules (conf))
(use-modules (tabdef))
(use-modules (bst))
(use-modules (db))

(define (main)
  (display-line "Starting sportsim...")

  (define n-countries (assoc-ref sportsim-conf 'n-countries))
  (define n-teams-per-country (assoc-ref sportsim-conf 'n-teams-per-country))
  (define db (create-db))
  (set! db (create-tab db 'country))
  (set! db (create-tab db 'team))

  (define (gen-teams db country-id)
    (let loop ((i-team 0))
      (when (< i-team n-teams-per-country)
        (let ()
          (define r
            (make-record 'team
              (name (string-append "team-" (number->string i-team)))
              (country-id country-id)))
          (set! db (db-insert! db 'team r))
          (loop (1+ i-team)))))
    db)

  (let loop ((i-country 0))
    (when (< i-country n-countries)
      (let ()
        (define r
          (make-record 'country
            (name (string-append "country-" (number->string i-country)))))
        (set! db (db-insert! db 'country r))
        (set! db (gen-teams db (record-value r 'id)))
        (loop (1+ i-country)))))

  ;(display-bst (assoc-ref db (list 'table 'country 'index '(id))))
  (display-line "Ending sportsim...")
)

(main)

;(define (create-entities! db tab-name n creator-proc)
;  (let loop ((i 0))
;    (when (< i n)
;      (let ((r (creator-proc i)))
;        (insert-record! db tab-name r)
;        (loop (+ i 1))))))
;
;(define (schedule-league-fixtures db current-date)
;  (display-line "start: schedule-league-fixtures")
;  (define countries (cdr (assoc (list 'table 'country 'data) db)))
;  (for-each
;    (lambda (r)
;      (schedule-league-fixtures-for-country
;        db
;        (record-attr country id r)
;        current-date))
;    (query-tab db 'country #:order-by '(name))))
;
;(define (do-matchup db event-record)
;  (display-line "*** Doing matchup:")
;  (display-line event-record))
;
;(define (schedule-league-fixtures-for-country db country-id current-date)
;  (define n-runs 2)
;  (define teams
;    (list->vector
;      (query-tab db 'team
;        #:pred
;          (lambda (r)
;            (equal? (record-attr team country-id r) country-id))
;        #:order-by '(name))))
;  (define n-teams (vector-length teams))
;  (define rounds (gen-round-robin n-teams))
;  (define round-date
;    (let loop ((round-date current-date))
;      (if (= (date->dow round-date) 6)
;        round-date
;        (loop (add-day round-date)))))
;  (define competition
;    (make-record competition (
;      (name
;        (string-append
;          "league-"
;          (number->string (date-year current-date))
;          "-"
;          country-id))
;      (country-id country-id)
;      (start-year (date-year current-date)))))
;  (let loop-over-runs ((round-date round-date) (i-run 0))
;    (unless (= i-run n-runs)
;      (let loop-over-rounds ((round-date round-date) (rounds rounds))
;        (unless (null? rounds)
;          (let loop-over-pairs ((pairs (car rounds)))
;            (unless (null? pairs)
;              (let* (
;                  (pair
;                    (
;                      (if (= (remainder i-run 2) 0)
;                        identity
;                        get-switched-pair)
;                      (car pairs)))
;                  (team-home-index (car pair))
;                  (team-away-index (cdr pair))
;                  (team-home (vector-ref teams team-home-index))
;                  (team-away (vector-ref teams team-away-index))
;                  (record
;                    (make-record event (
;                      (datetime round-date)
;                      (done? #f)
;                      (proc do-matchup)
;                      (team-home team-home)
;                      (team-away team-away)
;                      (competition-id
;                        (record-attr competition id competition))))))
;                (insert-record!
;                  db
;                  'event
;                  record)
;                (loop-over-pairs (cdr pairs)))))
;        (loop-over-rounds (add-days round-date 7) (cdr rounds)))
;      (loop-over-runs round-date (1+ i-run))))))
;
;(define (schedule-next-event db scheduled-item-record as-of-date)
;  (define next-date
;    (next-date-for-schedule as-of-date
;      (record-attr scheduled-item year scheduled-item-record)
;      (record-attr scheduled-item month scheduled-item-record)
;      (record-attr scheduled-item day scheduled-item-record)))
;  (when next-date
;    (insert-record!
;      db 'event
;      (make-record event (
;        (datetime next-date)
;        (done? #f)
;        (proc (record-attr scheduled-item proc scheduled-item-record))
;        (scheduled-item-id
;          (record-attr scheduled-item id scheduled-item-record)))))))
;
;(define (process-event db event-record)
;  (define proc (record-attr event proc event-record))
;  (proc db event-record)
;  (let (
;      (scheduled-item-id (record-attr event scheduled-item-id event-record)))
;    (when scheduled-item-id
;      (let (
;          (scheduled-item
;            (query-tab-by-id db 'scheduled-item scheduled-item-id)))
;        (when scheduled-item
;          (begin
;            (display-line "Scheduled-item:")
;            (display-line scheduled-item))))))
;  (record-set-attr! event done? event-record #t))
;
;(define (main)
;  (set! *random-state* (random-state-from-platform))
;  (format #t "Set random seed to ~a\n" (random-state->datum *random-state*))
;
;  (define db
;    (create-index
;    (create-tab
;    (create-tab
;    (create-tab
;    (create-tab
;    (create-tab
;    (create-db)
;    'player)
;    'country)
;    'team)
;    'event)
;    'scheduled-item)
;    'event '(datetime)))
;
;  (define start-date (assv-ref conf 'start-date))
;  (define stop-date (assv-ref conf 'stop-date))
;
;  ; Generate countries.
;  (let* ((n (assv-ref conf 'n-countries)))
;    (create-entities!
;      db
;      'country
;      n
;      (lambda (i)
;        (make-record country (
;          (name (string-append "country-" (number->string i))))))))
;
;  ; Generate teams and assign them to countries.
;  (let* (
;      (ntpc (assv-ref conf 'n-teams-per-country))
;      (nc (assv-ref conf 'n-countries))
;      (n (* ntpc nc))
;      (countries
;        (query-tab db 'country #:order-by '(id))))
;    (create-entities!
;      db
;      'team
;      n
;      (lambda (i)
;        (make-record team (
;          (name (string-append "team-" (number->string i)))
;          (country-id
;            (record-attr
;              country
;              id
;              (list-ref countries (remainder i nc)))))))))
;
;  ; Generate players and assign them to teams.
;  (let* (
;      (nppt (assv-ref conf 'n-players-per-team))
;      (ntpc (assv-ref conf 'n-teams-per-country))
;      (nc (assv-ref conf 'n-countries))
;      (n (* nc ntpc nppt)))
;    (create-entities!
;      db
;      'player
;      n
;      (lambda (i)
;        (make-record player (
;          (name (string-append "player-" (number->string i)))
;          (dob (date 2004 1 1))
;          (team-id (quotient i nppt))
;          (ratings #(50 50)))))))
;
;  (insert-record!
;    db 'scheduled-item
;    (make-record scheduled-item (
;      (year (date-year stop-date))
;      (month (date-month stop-date))
;      (day (date-day stop-date))
;      (proc exit))))
;  
;  (insert-record!
;    db 'scheduled-item
;    (make-record scheduled-item (
;      (month (date-month start-date))
;      (day (date-day start-date))
;      (proc
;        (lambda (db event-record)
;          (schedule-league-fixtures
;            db (record-attr event datetime event-record)))))))
;
;  (for-each
;    (lambda (r) (schedule-next-event db r start-date))
;    (query-tab db
;      'scheduled-item
;      #:order-by '(id)))
;
;  (let loop ((current-date start-date))
;    (let (
;        (next-events
;          (time (query-tab db 'event
;            #:pred
;              (lambda (e)
;                (equal? (record-attr event done? e) #f))
;            #:order-by
;              '(datetime)
;            ; Keep the limit at 1 so that an event can generate new events and
;            ; those events will be done in correct order.
;            #:limit 1))))
;      (if (null? next-events)
;        #f
;        (let ()
;          (for-each
;            (lambda (e)
;              (process-event db e))
;            next-events)
;          (loop current-date)))))
;
;)
;
;    ; Make a schedule data structure and fill it up periodically.
;
;(main)
