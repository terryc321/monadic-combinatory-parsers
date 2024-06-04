#|


-------------------------------------------------------------------------------

terry@debian:~/code/monadic-combinator-parsers/countdown/chicken-bug/smallest$ rlwrap mit-scheme --load small.scm
MIT/GNU Scheme running under GNU/Linux
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2022 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Wednesday January 11, 2023 at 11:12:18 PM
  Release 12.1 || SF || LIAR/x86-64
;Loading "small.scm"...
state = (21 25 30)
 v = 21 v2 = 30
 s3 = (21)           <<<<<<<<<<<<<<<<<<<<<<<<<<<<<   s3 contains 21 
 a = (21 21 30)
: b = (21 25 30)
v2 : v : s3 not equal? full state - ?
;halt
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.

2 error> 
End of input stream reached.
;... aborted

--------------------------------------------------------------------------------

Ceterum censeo Carthaginem esse delendam.
terry@debian:~/code/monadic-combinator-parsers/countdown/chicken-bug/smallest$ rlwrap mit-scheme --load small.scm
MIT/GNU Scheme running under GNU/Linux
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2022 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Wednesday January 11, 2023 at 11:12:18 PM
  Release 12.1 || SF || LIAR/x86-64
;Loading "small.scm"...
state = (21 25 30)
 v = 25 v2 = 21
 s3 = (25)         <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< s3 contains 25 
 a = (21 25 25)
: b = (21 25 30)
v2 : v : s3 not equal? full state - ?
;halt
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.

|#
