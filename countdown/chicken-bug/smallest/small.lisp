
(defpackage :small
  (:use :cl))

(in-package :small)


(defmacro inc! (x)
  `(setq ,x (+ ,x 1)))

(defmacro swap! (x y)
  (let ((tmp (gensym "tmp")))
    `(progn
       (let ((,tmp ,x))
	 (setq ,x ,y)
	 (setq ,y ,tmp)))))



(defun without2 (a xs zs)  
  (cond
   ((null xs) zs)
   ((equalp a (car xs)) (append (cdr xs) zs))
   (t (without2 a (cdr xs) (cons (car xs) zs)))))

(defun without3 (x ys)
  (without2 x ys '()))

(defun without1 (x ys)
  (cond
   ((null ys) ys)
   ((equalp x (car ys)) (cdr ys))
   (t (cons (car ys) (without1 x (cdr ys))))))


;; 
;; ;; do we have a sort - in all versions of scheme ?
(defun without1-3 (x ys)
   (let ((alpha (sort (copy-list (without1 x ys)) #'<)) ;; ...... 320 solutions 
	 (beta (sort (copy-list (without3 x ys)) #'<)))
    (cond
     ((equalp alpha beta) alpha)
     (t
      (format t "discrepancy without : ~a vs ~A ~%" alpha beta)
      (error "%without")))))


(defun without (x ys)
  ;;(without1 x ys))
   (without2 x ys '())) 
   ;; (without3 x ys)) ;; bad  
  ;;(without1-3 x ys))

  ;; (letrec ((without2 (lambda (a xs zs)
  ;; 		       (cond
  ;; 			((null? xs) zs)
  ;; 			((equal? a (car xs)) (append (cdr xs) zs))
  ;; 			(#t (without2 a (cdr xs) (cons (car xs) zs)))))))
  ;;   (without2 x ys '())))


;; a state = list values 1 3 5 7 10 25 50 
(defun entry ()
  (list (list 1 3 7 10 25 50) '()))

;; pick two numbers from state - remove them - generate a new number -
;; record what did


(defparameter *solution-count* 0)

(defun show-path (path)
    (dolist (p path)
      (format t "~a~%" p)))

(defun some-function (v v2 state s3 path target depth)
  (let ((add (+ v v2))
	(mul  (cond
	       ((or (= v 1)(= v2 1)) nil) ;; mul x 1 just x - no progress
	       (t (* v v2))))
	(div (let ((r nil))
	       (cond
		((= v 0) nil) ;; div 0 blow up
		((= v 1) nil) ;; div 3 1 just 3 no advantage
		((progn (setq r (/ v2 v)) (integerp r)) r)
		(t nil))))
	(sub (let ((r nil))
	       (cond
		((progn (setq r (- v2 v)) (> r 0)) r)
		(t nil)))))
    (when add
      (let ((s4 (cons add s3)))
	;;(format t "added ~a ~%" add)
	(some-search (+ depth 1) s4  add target
		(cons `((+ ,v2 ,v),s4) path))))
    (when sub
      (let ((s4 (cons sub s3)))
	;;(format t "added ~a ~%" sub)
	(some-search (+ depth 1) s4 sub target
		(cons `((- ,v2 ,v),s4) path))))
    (when div
      (let ((s4 (cons div s3)))
	;;(format t "added ~a ~%" div)			      
	(some-search (+ depth 1) s4 sub target
		(cons `((/ ,v2 ,v),s4) path))))
    (when mul
      (let ((s4 (cons mul s3)))
	;;(format t "added ~a ~%" mul)
	(some-search (+ depth 1) s4 mul target
		(cons `((* ,v2 ,v),s4) path))))
    ))


;; state : list of numbers 
(defun some-search (depth state num target path)
  (cond
    ((not (integerp num)) nil)
    ((= num target)
     (inc! *solution-count*)
     (format t "target : ~a : solution [~a] ~%" target *solution-count*)
     (show-path path)
     ;; if just want one solution - use a throw here
     ;;(throw 'exit path) 
     )
    (t
     (cond  ;; must have 2 numbers     
       ((< (length state) 2)  nil)
       (t 
	(dolist (v state)
	  (let ((s2 (without v state)))
	    (dolist (v2 s2)
	      ;;(let ((s3 (without v2 s2)))
	      (let ((s3 (without v2 (without v state))))

		;; sort in common lisp - obliterates variable
		;; so use copy-list before we sort ,
		;; can carry on using s3 and state variables
		(let ((a (sort (copy-list (cons v (cons v2 s3))) #'<))
		      (b (sort (copy-list state) #'<)))
		  (cond
		    ((equalp a b) 'ok)
		    (t
		     (format t " a = ~a : b = ~a ~%" a b )
		     (format t "took ~a ~a from ~a ended with ~a ~%" v v2 state s3)
		     (error "halt"))))
	
		;; make v smaller or equal v2
		
		;; <<<<< line below using swap! causes chaos >>>>>>>
		;; (when (> v v2)  (swap! v v2))

		;; <<<<< below same code as swap! to exclude wrote macro
		;;       incorrectly , discounted that idea because used
		;;       syntax-rules which is not complicated at all to use
		;; 
		;; --- to discount bug in macros we wrote a manual version
		;; --- same thing happens 
		;; (cond
		;;  ((> v v2) (let ((tmp v))
		;; 		  (setq v v2)
		;; 		  (setq v2 tmp))))
		(cond
		  ((> v v2)
		   (some-function v2 v state s3 path target depth))
		  (t
		   (some-function v v2 state s3 path target depth)))
		)))))))))


(defun run ()
  (catch 'exit 
    (let* ((num 1) ;;dummy not equal to target
	   (depth 1)
	   (target 765)
	   (puzzle '(1 3 7 10 25 30))
	   (path (list puzzle)))
      (setq *solution-count* 0)
      (some-search 
       depth
       puzzle
       num
       target
       path))))




(defun some-loop (a b)
  (format t "a = ~a : b = ~a ~%" a b)
  (swap! a b)
  (some-loop a b))


(defparameter *lop-steps* 0)

(defun some-loop2 (depth cs a b)
  (incf *lop-steps*)
  (cond
    ((> depth 6) nil)
    ((< (length cs) 2) nil)
   ;; ((> *lop-steps* 5) nil)
    (t 
     (dolist (a cs)
       (let ((xs (without a cs)))
	 (dolist (b xs)
	   (let ((ys (without b xs)))
	     (let ((cs2 (sort (copy-list (without b (without a cs))) #'<))
		   (cs3 (sort (copy-list ys) #'<)))
	       (when (not (equalp cs2 cs3))
		 (format t "~% - expectations - ~%")
		 (format t "cs2 = ~a~%" cs2)
		 (format t "cs3 = ~a~%" cs3)
		 (error "halt"))
	       (let ((a2 a)
		     (b2 b))
		 (swap! a2 b2)
		 ;;(swap! a b)
		 (format t "cs = ~a : a = ~a : b = ~a  : cs2 = ~a ~%" cs a2 b2 cs2)     
		 (some-loop2 (+ depth 1) cs2 a2 b2))))))))))











(defun lop2 ()
  (setq *lop-steps* 0)
  (some-loop2 0 '(1 3 5 7 10 25 50) 0 0))







(run)



#|

;;; when i do not use a swap! get 14088 solutions
target : 765 : solution [14088] 
((- 767 2) (765) NIL (767 2))
((+ 750 17) (767 2) (2) (2 17 750))
((- 3 1) (2 17 750) (17 750) (17 750 1 3))
((+ 10 7) (17 750 1 3) (750 1 3) (750 1 3 7 10))
((* 30 25) (750 1 3 7 10) (1 3 7 10) (1 3 7 10 25 30))
(1 3 7 10 25 30)

;;; when i do a swap! i get only 2562 solutions .....

target : 765 : solution [2562] 
((* 85 9) (765) NIL (85 9))
((- 175 90) (85 9) (9) (175 90 9))
((* 25 7) (175 90 9) (90 9) (90 9 7 25))
((* 30 3) (90 9 7 25) (9 7 25) (9 3 7 25 30))
((- 10 1) (9 3 7 25 30) (3 7 25 30) (1 3 7 10 25 30))
(1 3 7 10 25 30)

;; use a different without procedure
;; without2
target : 765 : solution [16933] 
((+ 750 15) (765) NIL (750 15))
((* 30 25) (750 15) (15) (15 30 25))
((+ 10 5) (15 30 25) (30 25) (5 30 25 10))
((- 7 2) (5 30 25 10) (30 25 10) (2 30 25 10 7))
((- 3 1) (2 30 25 10 7) (30 25 10 7) (1 3 7 10 25 30))
(1 3 7 10 25 30)




|#
