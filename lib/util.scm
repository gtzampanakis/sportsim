(use-modules (srfi srfi-19))

(define (is-leap? year)
  (cond
    ((= 0 (modulo year 400)) #t)
    ((= 0 (modulo year 100)) #f)
    ((= 0 (modulo year   4)) #t)
    (else                    #f)))

(define month-to-ndays (vector
  -1
  31 28 31 30
  31 30 31 31
  30 31 30 31))

(define (add-day date)
  (define day (date-day date))
  (define month (date-month date))
  (define year (date-year date))
  (define max-days
    (cond
      ((= month 2) (if (is-leap? year) 29 28))
      (else        (vector-ref month-to-ndays month))))
  (set! day (+ day 1))
  (if (> day max-days) (begin
      (set! day 1)
      (set! month (+ month 1))))
  (if (> month 12) (begin
    (set! month 1)
    (set! year (+ year 1))))
  (make-date 0 0 0 0 day month year 0))

(define (add-days date n-days)
  (cond
    ((= n-days 0) date)
    ((> n-days 0) (add-days (add-day date) (- n-days 1)))))

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

(define (dup-val-to-list v n)
  (if (= n 0)
    '()
    (cons v (dup-val-to-list v (- n 1)))))

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

(define (ts-to-date secs-since-1970)
  (let* (
      (start-year 2000)
      (secs-since-start-year (- secs-since-1970 TS2000))
      (q400 (floor-quotient secs-since-start-year secs-in-400-years))
      (r400 (floor-remainder secs-since-start-year secs-in-400-years))
      (p100 (div-irregular r400
        (cons
          (+ secs-in-std-4-years (* 24 secs-in-std-4-years))
          (dup-val-to-list (+ secs-in-non-std-4-years (* 24 secs-in-std-4-years)) 3))))
      (q100 (list-ref p100 0))
      (r100 (list-ref p100 1))
      (p4 (div-irregular r100
        (cons
          (if (= q100 0) secs-in-std-4-years secs-in-non-std-4-years)
          (dup-val-to-list secs-in-std-4-years 24))))
      (q4 (list-ref p4 0))
      (r4 (list-ref p4 1))
      (is-non-std? (and (= q4 0) (or (= q100 1) (= q100 2) (= q100 3))))
      (p1 (div-irregular r4
        (cons
          (if is-non-std? secs-in-non-leap-year secs-in-leap-year)
          (dup-val-to-list secs-in-non-leap-year 3))))
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
    (make-date 0 q-seconds q-minutes q-hours (+ 1 q-days) (+ 1 q-month) year 0)))
