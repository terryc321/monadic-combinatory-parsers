
;; ------------- chicken preambel --------------------------------------
(import scheme)
(import expand-full)
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

;; given a state - generate some new states
;; s : state
;; vals : numbers available
;; result : list of reachable states
;; es : expression stack - forth like interpreter
(define (next s)  
  (let* ((vals (car s))
	 (es (cadr s))
	 (result '())
	 (foo (lambda (v e) (set! result (cons (list v e) result)))))
    (dolist (v vals)
	    (let ((nes (cons v es)))
	      (foo (without v vals) nes)))
    (dolist (op '(+ - * /))
	    (let ((nes (cons op es)))
	      (foo vals nes)))
    result))


(define (stage-1) (next (entry)))
(define (stage-2) (apply append (map next (stage-1))))
(define (stage-3) (apply append (map next (stage-2))))
(define (stage-4) (apply append (map next (stage-3))))
(define (stage-5) (apply append (map next (stage-4))))
(define (stage-6) (apply append (map next (stage-5))))








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
		       






						  
