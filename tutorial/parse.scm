
(import scheme)
(import (chicken format))
(import srfi-1)
(import srfi-2)
(import (chicken pretty-print))
;;; scheme code for the parse

(define tests
  '(
    "vr{a>3414:A,R}"
    "lx{x>2140:A,A}"
    "vkm{s>411:A,a>443:R,A}"
    ))

#|
changelog

1) for sake simplicity assume all parse routines take a list of characters 
2) rather than typing parse-number at repl make shortcut aliases like pn
to mean parse-number
3) continuation passing style parser - see if it gives us what we want
coompare to other type of parsing
4) if success and failure continuation that makes next thing - exponential ?
 each try at something gives rise to two paths ...
5) note to self - cannot use quote ' mark as can in haskell , as quote ' in lisp
means something else entirely rather than x' meaning x prime variable its
variable x followed by an expected quotation
6)

bit long winded with 

cps parser
got a word -> success
no word -> failure ... 

functional parser ?
assume string is a sequence of characters like a list of characters ?

word may be A - to accept , R - to reject , another word - in which case go to that
workflow unconditionally 

word - bracket {  word  < > = INT  :COLON  word   ,COMMA?
                  word  < > = INT  :COLON  word    ,COMMA?
                  word  < > = INT  :COLON  word     ,COMMA?  
                  final-word 


peos  : parse end of string : match empty string - as in successfully parsed everything else
pw    : parse word
pnum  : parse number : integer in this case - converted to an integer also
pcol  : parse colon  : 
psemi : parse semicolon : 
pcom  : parse comma , 
pcb   : parse close bracket }
pob   : parse open bracket {
p>    : parse greater than
p<    : parse less than 

|#
(define (get-input)
  (let* ((port (open-input-file "lisp-input.scm"))
	 (result (read port)))
    (close-input-port port)
    result))


;; characters sequences split by { } , : 
(define (is-lower? ch)
  (and (char>=? ch #\a)
       (char<=? ch #\z)))

(define (is-upper? ch)
  (and (char>=? ch #\A)
       (char<=? ch #\Z)))

(define (is-digit? ch)
  (and (char>=? ch #\0)
       (char<=? ch #\9)))


;; end of string 
(define (parse-end-of-string xs scont fcont)
  (cond
   ((null? xs) (scont "" xs))
   (#t (fcont "nope" xs))))
(define peos parse-end-of-string)

(define (parse-equal-to xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\= ) (scont "=" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define p= parse-equal-to)


(define (parse-greater-than xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\> ) (scont ">" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define p> parse-greater-than)


(define (parse-less-than xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\< ) (scont "<" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define p< parse-less-than)


(define (parse-colon xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\: ) (scont ":" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define pcol parse-colon)


(define (parse-semicolon xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\; ) (scont ";" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define psemi parse-semicolon)


(define (parse-comma xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\, ) (scont "," (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define pcom parse-comma)


(define (parse-open-bracket xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\{ ) (scont "{" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define pob parse-open-bracket)

(define (parse-close-bracket xs scont fcont)
  (cond
   ((null? xs) (fcont "nope" xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((char=? ch #\} ) (scont "}" (cdr xs)))
	  (#t (fcont "nope" xs)))))))
(define pcb parse-close-bracket)


(define (parse-number-helper xs acc scont fcont)
  (cond
   ;;((null? acc) (fcont "no word" xs))
   ((null? xs) (scont (list->string (reverse acc)) xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((is-digit? ch) (parse-number-helper (cdr xs) (cons ch acc) scont fcont))
	  ((null? acc) (fcont "no digits" xs))
	  (#t (scont (list->string (reverse acc)) xs)))))))

;; just after successfully parsing a number - convert it to actual int using string->number
(define (parse-number xs scont fcont)
  (let ((scont2 (lambda (snum xs2)
		  (scont (string->number snum) xs2))))
  (parse-number-helper xs '() scont2 fcont)))
(define pnum parse-number)


(define (parse-word-helper xs acc scont fcont)
  (cond
   ;;((null? acc) (fcont "no word" xs))
   ((null? xs) (scont (list->string (reverse acc)) xs))
   (#t (let ((ch (car xs)))
	 (cond
	  ((is-lower? ch) (parse-word-helper (cdr xs) (cons ch acc) scont fcont))
	  ((is-upper? ch) (parse-word-helper (cdr xs) (cons ch acc) scont fcont))
	  ((null? acc) (fcont "no word" xs))
	  (#t (scont (list->string (reverse acc)) xs)))))))

;; when we parse word string - convert to actual lisp symbol and continue 
(define (parse-word xs scont fcont)
  (let ((scont2 (lambda (w xs)
		  (scont (string->symbol w) xs))))
  (parse-word-helper xs '() scont2 fcont)))
(define pw parse-word)

(define (parse-stage1 xs scont fcont)
  (pw xs scont fcont))

(define (parse xs scont fcont)
  (parse-stage1 xs scont fcont))
(define p parse)

(define sl string->list)
        
;; a  >  3414   :    A  
;; pw p> pnum  pcol  pw
(define (pw>n xs scont fcont)
  (let ((fail (lambda (m ys) (pw<n xs scont fcont))))
    (pw xs (lambda (w1 xs2)
    (p> xs2 (lambda (a> xs3)
    (pnum xs3 (lambda (n1 xs4)		      
    (pcol xs4 (lambda (bcol xs5)
    (pw xs5 (lambda (w2 xs6)
		 (scont `(if (> ,w1 ,n1) ,w2) xs6))fail))fail))fail))fail))fail)))


;; a  <  3414   :    A  
;; pw p< pnum  pcol  pw
(define (pw<n xs scont fcont)
  (let ((fail (lambda (m ys) (pw xs scont fcont))))
    (pw xs (lambda (w1 xs2)
    (p< xs2 (lambda (a< xs3)
    (pnum xs3 (lambda (n1 xs4)		      
    (pcol xs4 (lambda (bcol xs5)
    (pw xs5 (lambda (w2 xs6)
	      (scont `(if (< ,w1 ,n1) ,w2) xs6)) fail)) fail)) fail)) fail))fail)))


(define (pany xs scont fcont)
  (let ((fail (lambda (m ys) (pw xs scont fcont))))
    (pw>n xs (lambda (w1 xs2) ;; a > N : A  // a < N : A // a 
	       (pcom xs2 (lambda (_ xs3)
			   (pany xs3 (lambda (ws xs4)
				       (cond
					((symbol? ws)
					 (scont (cons w1 (list ws)) xs4))
					(#t 
					 (scont (cons w1 ws) xs4))))
				 fail))
		     (lambda (_ xs4)
		       (pcb xs2 (lambda (_ xs3)
				  (scont w1 xs2))
			    fail))))
	  fail)))



;; any number of pw<n or pw>n followed by pw 
;;  ,comma  
;; pcom

;; pw 


;; stage 1 parser .... how join continuations sequentially ? huh ...
(define (stage-1 str)
  (let ((xs (sl str))
	(fail (lambda (m ys) (list 'fail m ys))))
    (pw xs
	(lambda (w xs2) ;; word -> open bracket .... close bracket end-of-string
	  (pob xs2 (lambda (w2 xs3)
		     (pany xs3 (lambda (w3 xs4)
				 (list 'success 'got (cons w w3) 'remainder xs4))
			   (lambda (w3 xs4)
			     (ist 'fail 'got w3 'remiander xs4))))
	       fail))
	fail)))


(define st1 stage-1)





