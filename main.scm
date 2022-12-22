(use-modules (srfi srfi-9))
(use-modules (ice-9 format))

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

; (define p1
;   (make-player "p1" "2022-01-01" (list (make-merit 'att 10) (make-merit 'def 12))))
; (display p1)

(define (progress-date sim-state)
  '()
)

(define (main)
  (set! *random-state* (random-state-from-platform))
  (format #t "Set random seed to ~a\n" (random-state->datum *random-state*))

  ;(call-with-values
  ;  (lambda () (div-irregular 22 '(11 8 2 3 4)))
  ;  (lambda (p1 p2) (display p1) (newline) (display p2)))

  ; 11138548331 : Wednesday, December 20, 2322 9:12:11
  ;  1671545714 : Tuesday, December 20, 2022 14:15:14
  ;  1672496416 : Saturday, December 31, 2022 14:20:16 
  ;  1669904416 : Thursday, December 1, 2022 14:20:16 
  ;  951834016  : Tuesday, February 29, 2000 14:20:16 
  ;  983456416  : Thursday, March 1, 2001 14:20:16 
  ; 4107594016  : Monday, March 1, 2100 14:20:16
  ; 10418941216 : Thursday, March 1, 2300 14:20:16 
  ; 946684799   : Friday, December 31, 1999 23:59:59 
  ; -11670946784 : Tuesday, February 29, 1600 14:20:16
  ; 219099113524 : Thursday, December 22, 8912 8:52:04
  (display (ts-to-date 11138548331))(newline)
  (display (ts-to-date 1671545714))(newline)
  (display (ts-to-date 1672496416))(newline)
  (display (ts-to-date 1669904416))(newline)
  (display (ts-to-date 951834016))(newline)
  (display (ts-to-date 983456416))(newline)
  (display (ts-to-date 4107594016))(newline)
  (display (ts-to-date 10418941216))(newline)
  (display (ts-to-date 946684799))(newline)
  (display (ts-to-date -11670946784))(newline)
  (display (ts-to-date 219099113524))(newline)

)

(main)
