#|

Only change is without function
---------------------------------

(define (without x ys)
  (cond
   ((null? ys) ys)
   ((equal? x (car ys)) (cdr ys))
   (#t (cons (car ys) (without x (cdr ys))))))


(define (without x ys)
  (letrec ((without2 (lambda (a xs zs)
		       (cond
			((null? xs) zs)
			((equal? a (car xs)) (append (cdr xs) zs))
			(#t (without2 a (cdr xs) (cons (car xs) zs)))))))
    (without2 x ys '())))




terry@debian:~/code/monadic-combinator-parsers/countdown/chicken$ csc -O2 bug002-depth-first.scm 

Warning: (bug002-depth-first.scm:79) - assignment to imported value binding `pp'
terry@debian:~/code/monadic-combinator-parsers/countdown/chicken$ ./bug002-depth-first 
target : 765 : solution [1] (((+ 750 15) (765)) ((* 30 25) (750 15)) ((- 25 10) (15 25 30)) ((* 7 4) (28 30 25 10)) ((+ 3 1) (4 7 10 25 30)) (1 3 7 10 25 30)) 

(((+ 750 15) (765))
 ((* 30 25) (750 15))
 ((- 25 10) (15 25 30))
 ((* 7 4) (28 30 25 10))
 ((+ 3 1) (4 7 10 25 30))
 (1 3 7 10 25 30))

terry@debian:~/code/monadic-combinator-parsers/countdown/chicken$ csc -O3 bug002-depth-first.scm 

Warning: (bug002-depth-first.scm:79) - assignment to imported value binding `pp'
terry@debian:~/code/monadic-combinator-parsers/countdown/chicken$ ./bug002-depth-first 
target : 765 : solution [1] (((+ 750 15) (765)) ((* 30 25) (750 15)) ((- 25 10) (15 25 30)) ((* 7 4) (28 30 25 10)) ((+ 3 1) (4 7 10 25 30)) (1 3 7 10 25 30)) 

(((+ 750 15) (765))
 ((* 30 25) (750 15))
 ((- 25 10) (15 25 30))
 ((* 7 4) (28 30 25 10))
 ((+ 3 1) (4 7 10 25 30))
 (1 3 7 10 25 30))

terry@debian:~/code/monadic-combinator-parsers/countdown/chicken$ csc -O4 bug002-depth-first.scm 

Warning: (bug002-depth-first.scm:79) - assignment to imported value binding `pp'
terry@debian:~/code/monadic-combinator-parsers/countdown/chicken$ ./bug002-depth-first 
target : 765 : solution [1] (((+ 750 15) (765)) ((* 30 25) (750 15)) ((- 25 10) (15 25 30)) ((* 7 4) (28 30 25 10)) ((+ 3 1) (4 7 10 25 30)) (1 3 7 10 25 30)) 

(((+ 750 15) (765))
 ((* 30 25) (750 15))
 ((- 25 10) (15 25 30))
 ((* 7 4) (28 30 25 10))
 ((+ 3 1) (4 7 10 25 30))
 (1 3 7 10 25 30))

terry@debian:~/code/monadic-combinator-parsers/countdown/chicken$ csc -O5 bug002-depth-first.scm 

Warning: (bug002-depth-first.scm:79) - assignment to imported value binding `pp'
terry@debian:~/code/monadic-combinator-parsers/countdown/chicken$ ./bug002-depth-first 
target : 765 : solution [1] (((+ 750 15) (765)) ((* 30 25) (750 15)) ((- 25 10) (15 25 30)) ((* 7 4) (28 30 25 10)) ((+ 3 1) (4 7 10 25 30)) (1 3 7 10 25 30)) 

(((+ 750 15) (765))
 ((* 30 25) (750 15))
 ((- 25 10) (15 25 30))
 ((* 7 4) (28 30 25 10))
 ((+ 3 1) (4 7 10 25 30))
 (1 3 7 10 25 30))

|#

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
;;   define-macro
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


(define-macro (swap! x y)
  (let ((tmp (gensym "tmp")))
    `(begin
       (set! ,tmp ,x)
       (set! ,x ,y)
       (set! ,y ,tmp))))

(define-macro (inc! x)
  `(set! ,x (+ ,x 1)))



;; -------------------------------------------------------------------
#|

example 1 ;  -   1 3 7 10 25 50  target 765

|#

(define (without2 a xs zs)  
  (cond
   ((null? xs) zs)
   ((equal? a (car xs)) (append (cdr xs) zs))
   (#t (without2 a (cdr xs) (cons (car xs) zs)))))

(define (without3 x ys)
  (without2 x ys '()))

(define (without1 x ys)
  (cond
   ((null? ys) ys)
   ((equal? x (car ys)) (cdr ys))
   (#t (cons (car ys) (without1 x (cdr ys))))))


(define (without x ys)
  (let ((alpha (sort (without1 x ys) <))
	(beta (sort (without3 x ys) <)))
    (cond
     ((equal? alpha beta) alpha)
     (#t
      (format #t "discrepancy without : ~a vs ~A ~%" alpha beta)
      (error "without")))))



  ;; (letrec ((without2 (lambda (a xs zs)
  ;; 		       (cond
  ;; 			((null? xs) zs)
  ;; 			((equal? a (car xs)) (append (cdr xs) zs))
  ;; 			(#t (without2 a (cdr xs) (cons (car xs) zs)))))))
  ;;   (without2 x ys '())))

;; (without 1 '(1 1 2 3))
;; (without 2 '(1 1 2 3))
;; (without 3 '(1 1 2 3))

#|
#;1> (without 1 '(1 1 2 3))
(1 2 3)
#;26> (without 2 '(1 1 2 3))
(3 1 1)
#;28> (without 3 '(1 1 2 3))
(2 1 1)
#;30> (without 4 '(1 1 2 3))
(3 2 1 1)
#;31> (without 5 '(1 1 2 3))
(3 2 1 1)
|#


;; a state = list values 1 3 5 7 10 25 50 
(define (entry)
  (list (list 1 3 7 10 25 50) '()))

(define (/= x y) (not (= x y)))

;; pick two numbers from state - remove them - generate a new number -
;; record what did

(define solution-count 0)

;; state : list of numbers 
(define (search exit state num target path)
  ;;(format #t "state = ~a ~%" state)
  (cond
   ((not (integer? num)) #f)
   ((= num target)
    (inc! solution-count)
    (format #t "target : ~a : solution [~a] ~a ~%" target solution-count path)
    (newline)
    (pp path)
    (newline)
    (exit path)
    )
   (#t
    (dolist (v state)
	    (let ((s2 (without v state)))
	      (dolist (v2 s2)
		      (let ((s3 (without v2 s2)))
			(when (> v v2)
			  (swap! v v2))
			(let ((add (+ v v2))
			      (mul  (cond
				     ((or (= v 1)(= v2 1)) #f) ;; mul x 1 just x - no progress
				     (#t (* v v2))))
			      (div (let ((r #f))
				     (cond
				      ((= v 0) #f) ;; div 0 blow up
				      ((= v 1) #f) ;; div 3 1 just 3 no advantage
				      ((begin (set! r (/ v2 v)) (integer? r)) r)
				      (#t #f))))
			      (sub (let ((r #f))
				     (cond
				      ((begin (set! r (- v2 v)) (> r 0)) r)
				      (#t #f)))))
			  (when add
			    (let ((s4 (cons add s3)))
			    (search exit s4  add target (cons `((+ ,v2 ,v),s4) path))))
			  (when sub
			    (let ((s4 (cons sub s3)))
			    (search exit s4 sub target (cons `((- ,v2 ,v),s4) path))))
			  (when div
			    (let ((s4 (cons div s3)))
			    (search exit s4 sub target (cons `((/ ,v2 ,v),s4) path))))
			  (when mul
			    (let ((s4 (cons mul s3)))
			    (search exit s4 mul target (cons `((* ,v2 ,v),s4) path))))
			  ))))))))


(define (run)
  (call/cc (lambda (exit)
	     (let* ((num 1) ;;dummy not equal to target
		    (target 765)
		    (puzzle '(1 3 7 10 25 30))
		    (path (list puzzle)))
	       (set! solution-count 0)
	       (search exit
		       puzzle
		       num target path)))))




(run)


