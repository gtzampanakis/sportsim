(use-modules (srfi srfi-9))
(use-modules (srfi srfi-19))
(use-modules (ice-9 format))

(include "lib/util.scm")
(include "conf.scm")
(include "tabdef.scm")

(define (create-entities h n creator-proc)
  (let loop ((i 0))
    (when (< i n)
      (let ((e (creator-proc i)))
        (hashq-set! h i e)
        (loop (+ i 1))))))

(define (main)
  (define sch '())

  (set! *random-state* (random-state-from-platform))
  (format #t "Set random seed to ~a\n" (random-state->datum *random-state*))

  (let* (
      (ent-name-to-h (list
        (cons 'player (make-hash-table))
        (cons 'country (make-hash-table))
        (cons 'team (make-hash-table)))))

    (let* ((n (assv-ref conf 'n-countries)))
      (create-entities
        (assv-ref ent-name-to-h 'country)
        n
        (lambda (i)
          (vector i (number->string i)))))

    (let* (
        (ntpc (assv-ref conf 'n-teams-per-country))
        (nc (assv-ref conf 'n-countries))
        (n (* ntpc nc)))
      (create-entities
        (assv-ref ent-name-to-h 'team)
        n
        (lambda (i)
          (vector i (number->string i) (quotient i ntpc)))))

    (let* (
        (nppt (assv-ref conf 'n-players-per-team))
        (ntpc (assv-ref conf 'n-teams-per-country))
        (nc (assv-ref conf 'n-countries))
        (n (* nc ntpc nppt)))
      (create-entities
        (assv-ref ent-name-to-h 'player)
        n
        (lambda (i)
          (vector
            i
            (number->string i)
            (date 2004 1 1)
            (quotient i nppt)
            #(50 50)))))))

(main)
