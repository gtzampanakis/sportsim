(include "../lib/util.scm")

(define (test-ts->date test-fns)
  (let (
      (assert-equal (test-fns 'assert-equal))
      (tss (list
        11138548331 1671545714 1672496416
        1669904416 951834016 983456416
        4107594016 10418941216 219099113524
        -11670946784 946684799)))
    (for-each (lambda (ts) (
      display (assert-equal ts (date->ts (ts->date ts))))) tss)))
