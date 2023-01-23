(use-modules (srfi srfi-9))
(use-modules (srfi srfi-19))
(use-modules (ice-9 format))

(include "lib/util.scm")
(include "conf.scm")
(include "tabdef.scm")
(include "db.scm")

(define (create-entities! db tab-name n creator-proc)
  (let loop ((i 0))
    (when (< i n)
      (let ((r (creator-proc i)))
        (insert-record! db tab-name r)
        (loop (+ i 1))))))

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
          (vector i (number->string i)))))

    (let* (
        (ntpc (assv-ref conf 'n-teams-per-country))
        (nc (assv-ref conf 'n-countries))
        (n (* ntpc nc)))
      (create-entities!
        db
        'team
        n
        (lambda (i)
          (vector i (number->string i) (quotient i ntpc)))))
    
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
          (vector
            i
            (number->string i)
            (date 2004 1 1)
            (quotient i nppt)
            #(50 50)))))
    
    (let (
        (start-date (assv-ref conf 'start-date))
        (stop-date (assv-ref conf 'stop-date)))
      (let loop ((current-date start-date))
        (when (< (date->ts current-date) (date->ts stop-date))
          (format #t "Current date: ~a\n" current-date)
          (let (
              (month (date-month current-date))
              (day (date-day current-date)))
            (when (and (= month 8) (= day 1))
              'foo))
          (loop (add-day current-date)))))

))
      

    ; Make a schedule data structure and fill it up periodically.

(main)
