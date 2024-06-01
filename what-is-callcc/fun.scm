
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


(list
 (without 1 '(1 3 7 10 25 50))
 (without 3 '(1 3 7 10 25 50))
 (without 7 '(1 3 7 10 25 50))
 (without 10 '(1 3 7 10 25 50))
 (without 25 '(1 3 7 10 25 50))
 (without 50 '(1 3 7 10 25 50))
 ;; (without 100 '(1 3 7 10 25 50))
 )


;; (define (pick e vals target)
;;   (cond
;;    ((null? vals) #f)

(define foo-cont #f)
(define foo-cont-1 #f)
(define foo-cont-2 #f)
(define foo-cont-3 #f)
(define foo-cont-4 #f)
(define foo-cont-5 #f)

(define bar-cont #f)

;; toggles between them 
(define foo
  (let ((n 1))
    (lambda (next)
      (display "i am in foo ") (display n) (newline)
      (set! n (+ n 1))
      (cons 'a
	    (list 
	     (call/cc (lambda (k)
			(set! foo-cont-1 k)
			(k 1)))
	     (call/cc (lambda (k)
			(set! foo-cont-2 k)
			(k 1)))
	     (call/cc (lambda (k)
			(set! foo-cont-3 k)
			(k 1)))
	     (call/cc (lambda (k)
			(set! foo-cont-4 k)
			(k 1)))
	     (call/cc (lambda (k)
			(set! foo-cont-5 k)
			(k 1)))
	     )))))


      ;;(next foo))))

(define bar
  (let ((n 1))
    (lambda (next)
      (display "i am in bar ")(display n) (newline)
      (set! n (+ n 1))
      (call/cc (lambda (k) (set! bar-cont k)))
      (next bar))))





  


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
		       






						  
