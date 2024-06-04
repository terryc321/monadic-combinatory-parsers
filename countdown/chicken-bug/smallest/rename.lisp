

(defun some-loop (a b)
  (format t "a = ~a : b = ~a ~%" a b)
  (swap! a b)
  (some-loop a b))

      
(defun some-loop2 (depth as bs a b)
  (cond
    ((> depth 6) nil)
    (t 
     (format t "as = ~a : bs = ~a ~%" as bs)
     (dolist (a as)
       (dolist (b as)
	 (swap! a b)
	 (some-loop2 (+ depth 1) as bs a b))))))

(defun lop2 ()
  (some-loop2 0 '(1 3 5 7 10 25 50) '(1 3 5 7 10 25 50) 1 3))



