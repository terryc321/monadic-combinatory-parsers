
(defpackage :parse
  (:use :cl))

(in-package :parse)

;; again just google this ...
;; https://gigamonkeys.com/book/files-and-file-io
;; open input file and read lines of text
(defun read-input ()
  (let ((in (open "input" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do
	      (format t "(\"~a\")~%" line)
	      ;;(format t "parse -> ~a ~%" (parse line))
	      )	    
      (close in))))



(defun parse (s)
  ;; line start with { ... then onto stage 2 parsing
  (cond
    ((zerop (length s)) (empty-parse s))
    ((char= #\{ (char s 0)) (stage2-parse s))
    (t (stage1-parse s))))

(defun empty-parse (s)
  (format t "EMPTY LINE ~%"))


;; a to z continuous
(defun stage1-parse (s)
  (format t "~%STAGE 1 LINE : ~a ~%" s)
  (let ((i 0)
	(len (length s))
	(workflow ""))	
    (loop while (and (< i len) (let ((ch (char s i))) (and (char>= ch #\a) (char<= ch #\z)))) do
      (setq i (+ i 1)))
    (setq workflow (subseq s 0 i))
    (format t "workflow = ~a ~%" workflow)
    (format t "skip over ~A ~%" (char s i))
    (incf i)
    (let ((j i)
	  (tmp ""))
    (loop while (and (< i len) (let ((ch (char s i))) (and (char>= ch #\a) (char<= ch #\z)))) do
      (setq i (+ i 1)))
      (setq tmp (subseq s j i))
      (format t "tmp ~a ~%" tmp)
      )
    ))

    

(defun stage2-parse (s)
  (format t "STAGE 2 LINE ~%"))





