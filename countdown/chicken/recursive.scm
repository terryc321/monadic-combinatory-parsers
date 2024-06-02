
;; ------------- chicken preambel --------------------------------------
(import scheme)
(import expand-full) ;; expand*
(import simple-exceptions)
(import (chicken repl))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day17")
;; (get-current-directory)
(import procedural-macros)
(import regex)
(import simple-md5)
(import simple-loops)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val
;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors
;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))
(import sequences)
(import srfi-1)
(import matchable)
(define pp pretty-print)
(import srfi-69) ;; hash tables

;; (import-for-syntax
;;  (only procedural-macros macro-rules with-renamed-symbols once-only expand*))


;; ------------ macros ---------------------------------------
;; dolist
(define-macro (dolist varlist . body)
  (let ((var (car varlist))
	(ls (cadr varlist))
	(fn (gensym "fn")))	
    `(begin
       (letrec
	   ((,fn (lambda (xs)
		   (cond
		    ((null? xs) #f)
		    (#t (let ((,var (car xs)))
			  ,@body
			  (,fn (cdr xs))))))))
	 (,fn ,ls)))))

;; dofor
;; cannot handle decreasing steps ?
(define-macro (for v . body)
  (let ((var (car v))
	(init (cadr v))
	(lim (caddr v))
	(step (cadddr v))	      
	(foo (gensym "foo"))
	(v-i (gensym "i"))
	(v-step (gensym "step"))
	(v-lim (gensym "lim")))
    `(begin
       (letrec ;; want to capture var
	   ((,foo (lambda (,var ,v-step ,v-lim)
		    (cond
		     ((> ,var ,v-lim) #f)
		     (#t
		      ,@body
		      (,foo (+ ,var ,v-step) ,v-step ,v-lim))))))
	 (,foo ,init ,step ,lim)))))

;;(pp (expand* '(for (i 1 10 1) (format #t "i = ~A ~%" i))))
;; (for (i 1 10 1) (format #t "i = ~A ~%" i))
;; (for (i 10 1 -1) (format #t "i = ~A ~%" i))



(define-syntax compute-plus-macro
  (er-macro-transformer
   (lambda (expression rename comparator)
     `(cond
       ((>= (length ss) 2) (let ((b (car ss))
				 (a (cadr ss)))
			     (set! ss (cons (+ a b)
					    (cddr ss)))
			     (foo (cdr xs))))
       (#t (exit fail))))))


;;(pp (expand* (compute-plus-macro)))

(define-syntax compute-subtract-macro
  (er-macro-transformer
   (lambda (expression rename comparator)
  ;; - 
  `(cond
     ((>= (length ss) 2) (let ((b (car ss))
			       (a (cadr ss)))
			   (set! ss (cons (- a b)
					  (cddr ss)))
			   (foo (cdr xs))))
     (#t (exit fail))))))



(define-syntax compute-multiply-macro
  (er-macro-transformer
   (lambda (expression rename comparator)
  
  ;;  * 
  `(cond
     ((>= (length ss) 2) (let ((b (car ss))
			       (a (cadr ss)))
			   (set! ss (cons (* a b)
					  (cddr ss)))
			   (foo (cdr xs))))
     (#t (exit fail))))))


(define-syntax compute-divide-macro
  (er-macro-transformer
   (lambda (expression rename comparator)
  ;; / 
  `(cond
     ((>= (length ss) 2) (let ((b (car ss))
			       (a (cadr ss)))
			   (cond
			    ((= b 0) (exit fail))
			    (#t 
			     (set! ss (cons (/ a b)
					    (cddr ss)))
			     (foo (cdr xs))))))
     (#t (exit fail))))))





;; --------------------------------------------------------------------

#|

example 1 ;  -   1 3 7 10 25 50  target 765


|#

;; filter out a single x in xs
;; an error if without x xs , is called and x is not a member of xs 
(define (without x-orig xs-orig)
  (letrec ((without-helper (lambda (x xs ys)
			     (cond
			      ((null? xs) (error (list "without missing " x-orig xs-orig)))
			      ((equal? x (car xs)) (append (cdr xs) ys))
			      (#t (without-helper x (cdr xs) (cons (car xs) ys)))))))
    (without-helper x-orig xs-orig '())))

;; (define next #f)
;; 
;; ;; how about just pick n numbers in any sequence
;; (define foo (lambda ()
;; 	      (let ((n 1))
;; 		(set! next (lambda () (set! n (+ n 1)) n))
;; 		n)))

#|
from some set of reachable states

states -> all reachable next states 




|#

;; a state = list values 1 3 5 7 10 25 50 
(define (entry)
  (list (list 1 3 7 10 25 50) '()))


;; n : next states
;; s : states 
;; (define (next s)
;;   (next2 s '()))   

;; --- recursive version -----




;; forth stack is reverse of
;; depending on outcome
;; compute expression-stack ->  [   OK / #f = underflow , #t = leaves 1 item on stack  ,   VAL  ]
;;      [ #t #f 0 ]  means no underflow , left more than 1 item on stack , 0
;;    acceptable as patial stack may be [ 2 3 ] 
(define (compute es)
  (cond
   ((null? es) (list #t #f 0))
   (#t 
    (let* ((fs (reverse es))
	   (ss '())
	   (fail (list #f #f 0)))
      (call/cc (lambda (exit)
		 (letrec ((foo (lambda (xs)
				 (cond
				  ((null? xs) #t)
				  (#t (let ((x (car xs)))
					(cond
					 ((integer? x) (set! ss (cons x ss))
					  (foo (cdr xs)))
					 ;; + - 
					 ((eq? x '+) (compute-plus-macro))
					 ((eq? x '-) (compute-subtract-macro))
					 ((eq? x '*) (compute-multiply-macro))
					 ((eq? x '/) (compute-divide-macro))
					 (#t (error "compute bad operator" x)))))))))
		   (foo fs)
		   (cond
		    ((= (length ss) 1) (list #t #t (car ss)))
		    (#t (list #t #f 0))))))))))


(define attempts 0)
(define answers 0)


;; given a state - generate some new states
;; s : state
;; vals : numbers available
;; result : list of reachable states
;; es : expression stack - forth like interpreter
(define (next s len)  
  (let* ((vals (car s))
	 (es (cadr s))
	 (out-r (compute es))
	 (no-underflow (first out-r))
	 (complete (second out-r))
	 (value (third out-r)))
    ;;(format #t "s = ~A : len ~a ~%" s len)
    (set! attempts (+ 1 attempts))
    (cond
     ((> len 11) ;; limit unbounded recursion
      #f)
     ((not no-underflow) #f)     
     (#t
      ;; answer ?
      (when (and complete (= value 765))
	(set! answers (+ answers 1))
	(format #t "answer [~a / ~a] : solution ~a : ~a ~%" answers attempts (reverse es) 765))
      
      ;; just ensure build a good vals + expression stack 
      (dolist (v vals)
	      (let ((nes (cons v es)))
		(next (list (without v vals) nes)
		      (+ len 1))))
      
      (dolist (op '(+ - * /))
	      (let ((nes (cons op es)))
		(next (list vals nes)
		      (+ len 1))))
      ))))


(define (run)
  (next (entry) 0)
  (format #t "~%there were ~a attempts in total ~%" attempts))


;;; ----------- for compiler
(run)

#|

;;---------------------------------------------------------------------
with adjustments 2nd arg is on top stack , 1st arg is one under top of stack ,
a   : tos

b   : tos 
a

+    

(a+b) : top of stack 

answer [1129 / 176617565] : solution (50 1 + 25 10 + * 7 / 3 *) : 765 
answer [1130 / 176617655] : solution (50 1 + 25 10 + * 3 7 / *) : 765 
answer [1131 / 176617734] : solution (50 1 + 25 10 + * 3 * 7 /) : 765 
answer [1132 / 176621133] : solution (50 1 + 25 10 - *) : 765 
answer [1133 / 176631382] : solution (50 1 + 25 7 3 + - *) : 765 
answer [1134 / 176645262] : solution (50 1 + 25 7 - 3 - *) : 765 
answer [1135 / 176657815] : solution (50 1 + 25 3 7 + - *) : 765 
answer [1136 / 176671695] : solution (50 1 + 25 3 - 7 - *) : 765 
answer [1137 / 177570917] : solution (50 1 * 10 - 7 * 25 - 3 *) : 765 
answer [1138 / 178068238] : solution (50 1 / 10 - 7 * 25 - 3 *) : 765 

there were 178219001 attempts in total 
;; -----------------------------------------------------------------

answer [1 / 369397] : solution (1 3 7 50 10 - * 25 - * *) : 765 
(* (- (* 7 (- 50 10)) 25) 3)

50
7
3
1                                    


*** above changed arguments around 2nd item on stack is 1st ? 
this is ... well ...


answer [1126 / 176488305] : solution (50 1 + 10 25 + 7 3 / * *) : 765 
answer [1127 / 176488982] : solution (50 1 + 10 25 + 7 / 3 / *) : 765 
answer [1128 / 176489087] : solution (50 1 + 10 25 + 7 / / 3 /) : 765 
answer [1129 / 176491045] : solution (50 1 + 10 25 + * 7 3 / *) : 765 
answer [1130 / 176491150] : solution (50 1 + 10 25 + * 7 / 3 /) : 765 
answer [1131 / 176494713] : solution (50 1 + 10 25 - *) : 765 
answer [1132 / 176607198] : solution (50 1 + 25 10 3 7 - * - *) : 765 
answer [1133 / 176612737] : solution (50 1 + 25 10 + 7 3 / * *) : 765 
answer [1134 / 176613414] : solution (50 1 + 25 10 + 7 / 3 / *) : 765 
answer [1135 / 176613519] : solution (50 1 + 25 10 + 7 / / 3 /) : 765 
answer [1136 / 176615477] : solution (50 1 + 25 10 + * 7 3 / *) : 765 
answer [1137 / 176615582] : solution (50 1 + 25 10 + * 7 / 3 /) : 765 
answer [1138 / 176656608] : solution (50 1 + 25 3 7 - 10 * - *) : 765 

there were 178219001 attempts in total 
|#



;; (define (stage-1) (next (entry)))
;; (define (stage-2) (apply append (map next (stage-1))))
;; (define (stage-3) (apply append (map next (stage-2))))
;; (define (stage-4) (apply append (map next (stage-3))))
;; (define (stage-5) (apply append (map next (stage-4))))
;; (define (stage-6) (apply append (map next (stage-5))))


;; s : states
;; r : results
;; ;; if no more states to look at , r is resultant set
;; (define (next2 s r)  
;;   (cond
;;    ((null? s) r)
;;    (#t (let* ((st (car s))
;; 	      (vals (car st))
;; 	      (es (cadr st))
;; 	      (bar (lambda (y)
;; 		     (format #t "y = ~a ~%" y)
;; 		     (next2 (cdr s) (cons y r))))
;; 	      (foo (lambda (v e) (bar (list v e)))))
;; 	 ;; foo is what we do next after decide to put in
;; 	 (dolist (v vals)
;; 		 (let ((nes (cons v es)))
;; 		   (foo (without v vals) nes)))
;; 	 (dolist (op '(+ - * /))
;; 		 (let ((nes (cons op es)))
;; 		   (foo vals nes)))
;; 	 ))))

;; (list
;;  (without 1 '(1 3 7 10 25 50))
;;  (without 3 '(1 3 7 10 25 50))
;;  (without 7 '(1 3 7 10 25 50))
;;  (without 10 '(1 3 7 10 25 50))
;;  (without 25 '(1 3 7 10 25 50))
;;  (without 50 '(1 3 7 10 25 50))
;;  ;; (without 100 '(1 3 7 10 25 50))
;;  )


;; ;; (define (pick e vals target)
;; ;;   (cond
;; ;;    ((null? vals) #f)
;; 
;; (define foo-cont #f)
;; (define foo-cont-1 #f)
;; (define foo-cont-2 #f)
;; (define foo-cont-3 #f)
;; (define foo-cont-4 #f)
;; (define foo-cont-5 #f)
;; 
;; (define bar-cont #f)
;; 
;; ;; toggles between them 
;; (define foo
;;   (let ((n 1))
;;     (lambda (next)
;;       (display "i am in foo ") (display n) (newline)
;;       (set! n (+ n 1))
;;       (cons 'a
;; 	    (list 
;; 	     (call/cc (lambda (k)
;; 			(set! foo-cont-1 k)
;; 			(k 1)))
;; 	     (call/cc (lambda (k)
;; 			(set! foo-cont-2 k)
;; 			(k 1)))
;; 	     (call/cc (lambda (k)
;; 			(set! foo-cont-3 k)
;; 			(k 1)))
;; 	     (call/cc (lambda (k)
;; 			(set! foo-cont-4 k)
;; 			(k 1)))
;; 	     (call/cc (lambda (k)
;; 			(set! foo-cont-5 k)
;; 			(k 1)))
;; 	     )))))
;; 
;; 
;;       ;;(next foo))))
;; 
;; (define bar
;;   (let ((n 1))
;;     (lambda (next)
;;       (display "i am in bar ")(display n) (newline)
;;       (set! n (+ n 1))
;;       (call/cc (lambda (k) (set! bar-cont k)))
;;       (next bar))))





  


;; (map length '((3 7 10 25 50) (7 10 25 50 1) (10 25 50 3 1) (25 50 7 3 1) (50 10 7 3 1) (25 10 7 3 1)))
;; (5 5 5 5 5 5)
;; 
;; 
;; (map length '((3 7 10 25 50) (7 10 25 50 1) (10 25 50 3 1) (25 50 7 3 1) (50 10 7 3 1) (25 10 7 3 1) (50 25 10 7 3 1)))
;; 
;; (5 5 5 5 5 5 6)

;; stepping on toes of compiler / interpreter / imported libraries flip


;; (define cont #f)
;; (define myflip #f)
;; 
;; (define leaf? integer?)
;; (define node? pair?)
;; 
;; ;; traverse expression e and run procedure f 
;; (define (traverse e f)
;;   (cond
;;    ((leaf? e) 	(set! myflip (not myflip))
;; 		(call/cc (lambda (k) (set! cont k)))
;; 		(if myflip (f e) e))
;;    ((node? e) (list (car e)
;; 		   (traverse (cadr e) f)
;; 		   (traverse (caddr e) f)))
;;    (#t (error "traverse"))))
;; 
;; 
;; (traverse '(+ 1 2) (lambda (x) (list 'op x x)))
;; ;;  (+ (op 1 1) 2)
;; (cont #t)
;; ;;  (+ (op 1 1) #t)
		       






						  
