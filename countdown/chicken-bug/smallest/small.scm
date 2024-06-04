



;; r5rs compatibility
;; tested with scheme48 
;; tested with guile
;; tested with racket
;; tested with mitscheme
(define-syntax inc!
  (syntax-rules ()
    ((_ x) (set! x (+ x 1)))))

(define-syntax swap!
  (syntax-rules ()
    ((_ x y) (let ((tmp x))
	       (set! x y)
	       (set! y tmp)))))

;; rather than struggle with breaking hygiene just pass to a function one variable
;; supplied by user code , takes care of binding
;; (define-syntax dolist
;;   (syntax-rules ()
;;     ((_ ys f) (letrec ((foo (lambda (xs fun)
;; 			      (cond
;; 			       ((null? xs) '())
;; 			       (#t (fun (car xs))
;; 				   (foo (cdr xs) fun))))))
;; 		(foo ys f)))))


(define-syntax dolist
  (syntax-rules ()
    ((_ ys f) (letrec ((foo (lambda (xs)
			      (cond
			       ((null? xs) '())
			       (#t (f (car xs))
				   (foo (cdr xs)))))))
		(foo ys)))))




;;(dolist '() (lambda (x) (display "x = ") (display x) (newline)))
;;(dolist '(1 2 3 4) (lambda (x) (display "x = ") (display x) (newline)))
;;(dolist '(1 2 3 4 5 6 7 8 9 10) (lambda (x) (display "x = ") (display x) (newline)))


;; ----------- some common procedures below -------------

(define (/= x y) (not (= x y)))

;; ----------------------------------------------------------

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

;; do we have a sort - in all versions of scheme ?
(define (without1-3 x ys)
   (let ((alpha (sort (without1 x ys) <)) ;; ...... 320 solutions 
	(beta (sort (without3 x ys) <)))
    (cond
     ((equal? alpha beta) alpha)
     (#t
      (format #t "discrepancy without : ~a vs ~A ~%" alpha beta)
      (error "%without")))))


(define (without x ys)
  ;;(without1 x ys)) ;; ok ....... 1072 solutions  ... apparently 14088 solutions ...
  (without2 x ys '())) ;; bad   ............ 14088 solutions 
   ;; (without3 x ys)) ;; bad  
  ;;(without1-3 x ys))

  ;; (letrec ((without2 (lambda (a xs zs)
  ;; 		       (cond
  ;; 			((null? xs) zs)
  ;; 			((equal? a (car xs)) (append (cdr xs) zs))
  ;; 			(#t (without2 a (cdr xs) (cons (car xs) zs)))))))
  ;;   (without2 x ys '())))

;; a state = list values 1 3 5 7 10 25 50 
(define (entry)
  (list (list 1 3 7 10 25 50) '()))

;; pick two numbers from state - remove them - generate a new number -
;; record what did

(define solution-count 0)

(define (show-path path)
  (dolist path (lambda (p) (display p) (newline))))

(define (some-function v v2 state s3 path target depth)
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
	;;(format #t "added ~a ~%" add)
	(search exit (+ depth 1) s4  add target
		(cons `((+ ,v2 ,v),s4 ,s3 ,state) path))))
    (when sub
      (let ((s4 (cons sub s3)))
	;;(format #t "added ~a ~%" sub)
	(search exit (+ depth 1) s4 sub target
		(cons `((- ,v2 ,v),s4 ,s3 ,state) path))))
    (when div
      (let ((s4 (cons div s3)))
	;;(format #t "added ~a ~%" div)			      
	(search exit (+ depth 1) s4 sub target
		(cons `((/ ,v2 ,v),s4 ,s3 ,state) path))))
    (when mul
      (let ((s4 (cons mul s3)))
	;;(format #t "added ~a ~%" mul)
	(search exit (+ depth 1) s4 mul target
		(cons `((* ,v2 ,v),s4 ,s3 ,state) path))))
    ))


;; state : list of numbers 
(define (search exit depth state num target path)
  (cond
   ((not (integer? num)) #f)
   ((= num target)
    (inc! solution-count)

    ;; format non portable
    ;; (format #t "target : ~a : solution [~a] ~a ~%" target solution-count path)
    
    (display "target : ")
    (display target)
    (display " : solution [")
    (display solution-count)
    (display "] ")
    (newline)
    (show-path path)
    (newline)

    (newline)
    ;; pretty-print non portable
    ;;(pp path)
    (display path)
    (newline)    

    ;;(exit path) ;; finish after one solution found

    )
   (#t
    (cond  ;; must have 2 numbers     
     ((< (length state) 2)  #f)
     (#t 
    (dolist state (lambda (v)
		    (let ((s2 (without v state)))
		      (dolist s2 (lambda (v2)
				   (let ((s3 (without v2 s2)))

				     (let ((a (sort (cons v2 (cons v s3)) <))
					   (b (sort state <)))
				       (cond
					((equal? a b) 'ok)
					(#t
					 (newline)
					 (display "state = ") (display state)
					 (newline)
					 (display " v = ") (display v)
					 (display " v2 = ") (display v2)
					 (newline)
					 (display " s3 = ") (display s3)
					 (newline)
					 (display " a = " ) (display a) (newline)
					 (display ": b = ") (display b)(newline)
					 (display "v2 : v : s3 not equal? full state - ?")
					 (newline)
					 (error "halt"))))
				     ;; make v smaller or equal v2
				     
				     ;; <<<<< line below using swap! causes chaos >>>>>>>
				     ;;(when (> v v2)  (swap! v v2))

				     ;; <<<<< below same code as swap! to exclude wrote macro
				     ;;       incorrectly , discounted that idea because used
				     ;;       syntax-rules which is not complicated at all to use
				     ;; 
				     ;; --- to discount bug in macros we wrote a manual version
				     ;; --- same thing happens 
				     ;; (cond
				     ;;  ((> v v2) (let ((tmp v))
				     ;; 		  (set! v v2)
				     ;; 		  (set! v2 tmp))))
				     
				     
				     (cond
				      ((> v v2)
				       (some-function v2 v state s3 path target depth))
				      (#t
				       (some-function v v2 state s3 path target depth)))
				     )))))))))))







(define (run)
  (call/cc (lambda (exit)
	     (let* ((num 1) ;;dummy not equal to target
		    (depth 1)
		    (target 765)
		    (puzzle '(1 3 7 10 25 30))
		    (path (list puzzle)))
	       (set! solution-count 0)
	       (search exit
		       depth
		       puzzle
		       num target path)))))


(run)


