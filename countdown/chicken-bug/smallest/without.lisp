


(defparameter *error-count* 0)
(defparameter *steps-count* 0)

(defmacro swap! (x y)
  (let ((tmp (gensym "tmp")))
    `(progn
       (let ((,tmp ,x))
	 (setq ,x ,y)
	 (setq ,y ,tmp)))))

(defun without (x ys)
  (cond
   ((null ys) ys)
   ((equalp x (car ys)) (cdr ys))
   (t (cons (car ys) (without x (cdr ys))))))


(defun example-1 ()
  (let ((xs '(1 2 3 5 7 10 25 50)))
    (dolist (a xs)
      (let ((a0 a))
      (let ((ys (without a xs)))
	(dolist (b ys)
	  (let ((b0 b))
	    (let ((zs (without b ys)))
	                                      ;;;  ***  a was a , now b
	                                      ;;; removing old-b with new-b rather than a new-b
	    (let ((check1 (sort (copy-list (without a (without b xs))) #'<))
		  (check2 (sort (copy-list zs) #'<)))
	      (format t "a = ~a : b = ~a : zs = ~a : xs = ~a : ys = ~a~%" a b zs xs ys)
	      ;; here - below - is the line i cannot understand
	      (swap! a b)
	      (incf *steps-count*)
	      (when (not (equalp check1 check2))
		(incf *error-count*)
		(format t "error ~a : check1 = ~a : check2 = ~a ~%" *error-count* check1 check2)
	      	(error "halt")
		)
	      )))))))))


(defun run ()
  (setq *error-count* 0)
  (setq *steps-count* 0)  
  (example-1)
  (list *error-count* *steps-count*))





