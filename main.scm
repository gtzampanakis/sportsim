(use-modules (srfi srfi-9))
(use-modules (srfi srfi-19))
(use-modules (ice-9 format))

(include "lib/util.scm")
(include "conf.scm")

(define (create-players! players-hash)
  (format #t "Creating players...\n")
  (let loop (
      (i (assv-ref conf 'n-players)))
    (when (> i 0)
      (let (
          (p (list
            (cons 'id 1)
            (cons 'name (number->string i))
            (cons 'dob (date 2004 1 1)))))
        (hashq-set! players-hash i p)
        (loop (- i 1))))))

(define (create-countries! countries-hash)
  (format #t "Creating countries...\n")
  (let loop (
      (i (assv-ref conf 'n-countries)))
    (when (> i 0)
      (let (
          (c (list
            (cons 'id 1)
            (cons 'name (number->string i)))))
        (display c)
        (display "\n")
        (hashq-set! countries-hash i c)
        (loop (- i 1))))))

(define (main)
  (define sch '())

  (set! *random-state* (random-state-from-platform))
  (format #t "Set random seed to ~a\n" (random-state->datum *random-state*))

  (let (
      (players-hash (make-hash-table))
      (teams-hash (make-hash-table))
      (countries-hash (make-hash-table)))
    (create-players! players-hash)
    (create-countries! countries-hash)))

(main)
