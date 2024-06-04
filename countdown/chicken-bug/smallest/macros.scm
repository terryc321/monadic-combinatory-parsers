


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
(define-syntax dolist
  (syntax-rules ()
    ((_ ys f) (letrec ((foo (lambda (xs fun)
			      (cond
			       ((null? xs) '())
			       (#t (let ((val (fun (car xs))))
				     (cond
				      ((null? (cdr xs)) val)
				      (#t
				       (foo (cdr xs) fun)))))))))
		(foo ys f)))))


;;(dolist '() (lambda (x) (display "x = ") (display x) (newline)))
;;(dolist '(1 2 3 4) (lambda (x) (display "x = ") (display x) (newline)))
;;(dolist '(1 2 3 4 5 6 7 8 9 10) (lambda (x) (display "x = ") (display x) (newline)))


;; ----------- some common procedures below -------------

(define (/= x y) (not (= x y)))
