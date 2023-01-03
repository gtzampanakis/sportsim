(use-modules (srfi srfi-9))
(use-modules (ice-9 format))

(include "lib/test-runner.scm")
(include "lib/util.scm")
(include "conf.scm")

(define-record-type <merit>
  (make-merit name val)
  merit?
  (name merit-name set-merit-name!)
  (val merit-val set-merit-val!)
)

(define-record-type <player>
  (make-player name dob merits)
  player?
  (name player-name set-player-name!)
  (dob player-dob set-player-dob!)
  (merits player-merits set-player-merits!)
)

(define-record-type <team>
  (make-team players)
  team?
  (players team-players set-team-players!)
)

(define-record-type <world>
  (make-world current-date)
  world?
  (current-date world-current-date set-world-current-date!)
)

(define (progress-date sim-state)
  '()
)

(define (main)
  (set! *random-state* (random-state-from-platform))
  (format #t "Set random seed to ~a\n" (random-state->datum *random-state*))

)

(main)
