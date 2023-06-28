(define-module (lib util)
  #:export (
    date=?
    date<?
    date<=?
    date>?
    date>=?))

(use-modules (srfi srfi-19))

(define-public (display-line obj)
  (display obj)(newline))

(define-public (display-list ls)
  (for-each
    (lambda (e) (display e)(newline))
    ls))

(define (div-irregular a bs)
  ; Returns (r1 . r2)
  ; where a = (sum bs[:r1]) + r2
  (if (equal? bs '())
    (list 0 a)
    (let ((b (car bs)))
      (if (< a b)
        (list 0 a)
        (let* (
          (p (div-irregular (- a b) (cdr bs)))
          (r1 (list-ref p 0))
          (r2 (list-ref p 1)))
            (list (+ 1 r1) r2))))))

; TS2000 : Timestamp at 200-01-01
(define TS2000  (* (+ 7 (* 30 365)) 24 60 60))
(define secs-in-day (* 24 60 60))
(define days-in-std-4-years (+ 1 (* 4 365)))
(define days-in-400-years (- (* 100 days-in-std-4-years) 3))
(define secs-in-400-years (* days-in-400-years secs-in-day))
(define secs-in-non-leap-year (* secs-in-day 365))
(define secs-in-leap-year (* secs-in-day 366))
(define secs-in-std-4-years (* secs-in-day days-in-std-4-years))
(define secs-in-non-std-4-years (* secs-in-day 365 4))

(define-public (ts->date secs-since-1970)
  (let* (
      (start-year 2000)
      (secs-since-start-year (- secs-since-1970 TS2000))
      (q400 (floor-quotient secs-since-start-year secs-in-400-years))
      (r400 (floor-remainder secs-since-start-year secs-in-400-years))
      (p100 (div-irregular r400
        (cons
          (+ secs-in-std-4-years (* 24 secs-in-std-4-years))
          (make-list
            3 (+ secs-in-non-std-4-years (* 24 secs-in-std-4-years))))))
      (q100 (list-ref p100 0))
      (r100 (list-ref p100 1))
      (p4 (div-irregular r100
        (cons
          (if (= q100 0) secs-in-std-4-years secs-in-non-std-4-years)
          (make-list 24 secs-in-std-4-years))))
      (q4 (list-ref p4 0))
      (r4 (list-ref p4 1))
      (is-non-std? (and (= q4 0) (or (= q100 1) (= q100 2) (= q100 3))))
      (p1 (div-irregular r4
        (cons
          (if is-non-std? secs-in-non-leap-year secs-in-leap-year)
          (make-list 3 secs-in-non-leap-year))))
      (q1 (list-ref p1 0))
      (r1 (list-ref p1 1))
      (leap? (and (not is-non-std?) (= q1 0)))
      (days-per-month (list 31 (if leap? 29 28) 31 30 31 30 31 31 30 31 30 31))
      (p-month (div-irregular r1
        (map (lambda (d) (* d secs-in-day)) days-per-month)))
      (q-month (list-ref p-month 0))
      (r-month (list-ref p-month 1))
      (q-days (quotient r-month secs-in-day))
      (r-days (remainder r-month secs-in-day))
      (q-hours (quotient r-days 3600))
      (r-hours (remainder r-days 3600))
      (q-minutes (quotient r-hours 60))
      (r-minutes (remainder r-hours 60))
      (q-seconds r-minutes)
      (year (+ start-year (* 400 q400) (* 100 q100) (* 4 q4) q1)))
    (make-date
      0 q-seconds q-minutes q-hours (+ 1 q-days) (+ 1 q-month) year 0)))

(define-public (date->ts date)
  (let* (
      (start-year 2000)
      (years-since-start-year (- (date-year date) start-year))
      (q400 (floor-quotient years-since-start-year 400))
      (r400 (floor-remainder years-since-start-year 400))
      (q100 (quotient r400 100))
      (r100 (remainder r400 100))
      (q4 (quotient r100 4))
      (r4 (remainder r100 4))
      (q1 r4)
      (non-leap? (or (> q1 0) (and (= q4 0) (> q100 0))))
      (month (date-month date))
      (days-per-month
        (list 31 (if non-leap? 28 29) 31 30 31 30 31 31 30 31 30 31))
      (day (date-day date))
      (hour (date-hour date))
      (minute (date-minute date))
      (second (date-second date)))
    (+
      TS2000
      (* q400 secs-in-400-years)
      (cond
        ((= q100 0) 0)
        (else (+ (*
            (- q100 1)
            (+ secs-in-non-std-4-years (* 24 secs-in-std-4-years))
          ) (* 25 secs-in-std-4-years))))
      (cond
        ((= q4 0) 0)
        (else (+ (*
            (- q4 1) secs-in-std-4-years)
          (if (= q100 0) secs-in-std-4-years secs-in-non-std-4-years))))
      (cond
        ((= q1 0) 0)
        (else (+
          (* (- q1 1) secs-in-non-leap-year)
          (if (and (= q4 0) (> q100 0))
            secs-in-non-leap-year
            secs-in-leap-year))))
      (* secs-in-day
        (let calc-month ((m month) (dpm days-per-month))
          (cond
            ((= m 1) 0)
            (else (+ (car dpm) (calc-month (- m 1) (cdr dpm)))))))
      (* secs-in-day (- day 1))
      (* 3600 hour)
      (* 60 minute)
      second)))

(define-public (ts->dow ts)
  (define r1 (floor-quotient ts secs-in-day))
  (define dow (remainder (+ r1 3) 7))
  dow)

(define-public (date->dow date)
  (ts->dow (date->ts date)))

(define-public (leap? year)
  (cond
    ((= (remainder year 400) 0) #t)
    ((= (remainder year 100) 0) #f)
    ((= (remainder year 4) 0) #t)
    (else #f)))

(define-public (add-days date days)
  (ts->date (+ (* days 3600 24) (date->ts date))))

(define-public (add-day date)
  (add-days date 1))

(define-public (valid-date? date-in)
  (define year (date-year date-in))
  (define month (date-month date-in))
  (define day (date-day date-in))
  (and
    (not (= year 0))
    (>= month 1)
    (<= month 12)
    (>= day 1)
    (<=
      day
      (cond
        ((= month 1) 31)
        ((= month 2) (if (leap? year) 29 28))
        ((= month 3) 31)
        ((= month 4) 30)
        ((= month 5) 31)
        ((= month 6) 30)
        ((= month 7) 31)
        ((= month 8) 31)
        ((= month 9) 30)
        ((= month 10) 31)
        ((= month 11) 30)
        ((= month 12) 31)))))

(define-public (add-months date-in months)
  (define year (date-year date-in))
  (define month (date-month date-in))
  (define day (date-day date-in))
  (define-values (q r) (floor/ (+ months (1- month)) 12))
  (define new-month (1+ r))
  (define new-year (+ year q))
  (define candidate (date new-year new-month day))
  (if (valid-date? candidate) candidate #f))

(define-public (add-years date-in years)
  (define year (date-year date-in))
  (define month (date-month date-in))
  (define day (date-day date-in))
  (define straddle-zero-year? (and (< year 0) (>= (+ year years) 0)))
  (define new-year (+ year (+ years (if straddle-zero-year? 1 0))))
  (define candidate (date new-year month day))
  (define result (if (valid-date? candidate) candidate #f))
  result)

(define-public (date y m d)
  (make-date 0 0 0 0 d m y 0))

(define-public (list-insert ls ind obj)
  (let loop ((ls ls) (ind ind) (prefix '()))
    (append prefix
      (cond
        ((null? ls) (list obj))
        ((= ind 0) (cons obj ls))
        ((= ind 1) (cons (car ls) (cons obj (cdr ls))))
        (else (loop (cdr ls) (1- ind) (list (car ls))))))))

(define-public (gen-rand-perm n)
  (let loop ((result '()) (i 0))
    (if (= i n)
      result
      (loop (list-insert result (random (1+ i)) i) (1+ i)))))

(define-public (get-switched-pair p)
  (cons (cdr p) (car p)))

(define-public (gen-round-robin n)
  ; See https://en.wikipedia.org/wiki/Round-robin_tournament#Circle_method
  (when (<= n 0)
    (error "gen-round-robin: n should be greater than 0"))
  (define n-1 (1- n))
  (define n/2-1 (1- (/ n 2)))
  (define first-round
    (let loop-to-gen-first-round ((i 0) (result (list (cons 0 n-1))))
      (if (= i n/2-1)
        result
        (let ((prev (car result)))
          (loop-to-gen-first-round
            (1+ i)
            (cons (cons (1+ (car prev)) (1- (cdr prev))) result))))))
  (define (cycle number)
    (cond
      ((= number 0) 0)
      ((= number 1) n-1)
      (else (1- number))))
  (define (cycle-pair p)
    (cons (cycle (car p)) (cycle (cdr p))))
  (define rounds
    (let loop-to-gen-other-rounds ((rounds (list first-round)))
      (define (last-home-away number)
        (let loop ((pairs (car rounds)))
          (let ((pair (car pairs)))
            (cond
              ((= number (car pair)) 'h)
              ((= number (cdr pair)) 'a)
              (else (loop (cdr pairs)))))))
      (define (get-switch-paired-if-needed p)
        ; Try to alternate home-away for each team on each round.
        (let (
            (a (last-home-away (car p)))
            (b (last-home-away (cdr p))))
          (cond
            ((and (eqv? a 'h) (eqv? b 'a)) (get-switched-pair p))
            ((and (eqv? a 'a) (eqv? b 'h)) p)
            (else p))))
      (if (= (length rounds) n-1)
        rounds
        (loop-to-gen-other-rounds
          (cons
            (map get-switch-paired-if-needed (map cycle-pair (car rounds)))
            rounds)))))
  rounds)

(define compare-dates-procs (list
  date-year
  date-month
  date-day
  date-hour
  date-minute
  date-second
  date-nanosecond))

(define-public (compare-dates d1 d2)
  (let loop ((procs compare-dates-procs))
    (if (null? procs)
      0
      (let* (
          (proc (car procs))
          (v1 (proc d1))
          (v2 (proc d2))
          (diff (- v1 v2)))
        (if (= diff 0)
          (loop (cdr procs))
          diff)))))

(define-syntax date=?
  (syntax-rules ()
    ((_ d1 d2) (= (compare-dates d1 d2) 0))))

(define-syntax date<?
  (syntax-rules ()
    ((_ d1 d2) (< (compare-dates d1 d2) 0))))

(define-syntax date<=?
  (syntax-rules ()
    ((_ d1 d2) (<= (compare-dates d1 d2) 0))))

(define-syntax date>?
  (syntax-rules ()
    ((_ d1 d2) (> (compare-dates d1 d2) 0))))

(define-syntax date>=?
  (syntax-rules ()
    ((_ d1 d2) (>= (compare-dates d1 d2) 0))))

(define (date-matches? date-in year month day)
  (and
    (or (null? year) (= (date-year date-in) year))
    (or (null? month) (= (date-month date-in) month))
    (or (null? day) (= (date-day date-in) day))))

(define-public (max-date-that-matches year month day)
  (cond
    ((null? year) #f)
    ((and (not (null? month)) (not (null? day))) (date year month day))
    ((and (not (null? month)) (null? day))
      (add-days (add-months (date year month 1) 1) -1))
    ((and (null? month) (not (null? day)))
      (date year 12 day))
    ((and (null? month) (null? day))
      (date year 12 31))))

(define-public
    (next-date-for-schedule as-of-date
      schedule-year schedule-month schedule-day)
  (define max-date-for-schedule
    (max-date-that-matches schedule-year schedule-month schedule-day))
  (define result
    (let loop ((as-of-date as-of-date))
      (if (and
            (date? max-date-for-schedule)
            (date>? as-of-date max-date-for-schedule))
        #f
        (let ()
          (if (date-matches?
                as-of-date schedule-year schedule-month schedule-day)
            as-of-date
            (loop (add-day as-of-date)))))))
  result)

(define-public (join dlm . strings)
  (cond
    ((null? strings) "")
    ((null? (cdr strings)) (car strings))
    (else
      (string-append
        (car strings)
        dlm
        (apply join (cons dlm (cdr strings)))))))

(define-public (find-in-cache cache key)
  (if (null? cache)
    '---not-found-c96020fd---
    (let ((pair (car cache)))
      (if (equal? (car pair) key)
        (cdr pair)
        (find-in-cache (cdr cache) key)))))

(define-public (memoized-proc proc)
  (define cache '())
  (lambda args
    (define from-cache (find-in-cache cache args))
    (if (equal? from-cache '---not-found-c96020fd---)
      (let ((r (apply proc args)))
        (set! cache (cons (cons args r) cache))
        r)
      from-cache)))
