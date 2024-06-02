
* Countdown puzzle 

```lisp
with six numbers 1 2 3 4 5 6 in forth postfix notation 
1 2 3 4 5 6 +
1 2 3 4 (5+6) +
1 2 3 (4+5+6) +
1 2 (3+4+5+6) +
1 (2+3+4+5+6) +
(1+2+3+4+5+6) 

six numbers and five operators , max length of forth expression is eleven items 

using this technique to randomly hope and spray operator or a value from six numbers

...
answer [1136 / 176671695] : solution (50 1 + 25 3 - 7 - *) : 765 
answer [1137 / 177570917] : solution (50 1 * 10 - 7 * 25 - 3 *) : 765 
answer [1138 / 178068238] : solution (50 1 / 10 - 7 * 25 - 3 *) : 765 

there were 178219001 attempts in total 
real	1m3.265s
user	1m3.047s
sys	0m0.216s

```





Haskell puzzle on a haskell (hutton nottingham university) youtube 

From tv show countdown 

given 5 positive integers  a , b , c  , d , e 

using four operators + add , - subtract , * multiply , / divide

given some positive integer to use as a target T

find a way to make T from a - e using four operators 

some restrictions also , 

- cannot have negative numbers ? ... more on this later 

- cannot have fractional values like 2 / 3 only whole positive integers

Can we find a haskell solution ? 

example in hutton video 1 , 3 , 7 , 10 , 25 , 50  and target 765

* Ideas

```lisp

#|

countdown lisp

5 numbers

example 1 ;  -   1 3 7 10 25 50  target 765

using four operators + - * / 

what are all possible combinations

thinking .......

shall we assume the 5 numbers are sorted ? low to high ?

do we allow repeated numbers ?
-------------------------------------------------------------
question 1 : what numbers can i make from one number ?
the number itself

----------------------------------------------------------
question 2 : what number can i make from two numbers ?
number a itself
number b itself
choice of four operators and a b

a + b
a * b
a - b
a / b

b + a  -- duplicate of a + b 
b - a
b / a
b * a   -- duplicate of a * b 

if a = b then a - b is zero , so invalid step
a / b would yield 1 , might be useful

-------------------------------------------------------------
question 3 : three numbers

from set numbers produced by question 2 -
simply question 2 asked again , except now the two numbers are
one from set of numbers produced by question 2 , the number picked in question 3 

--------------------------------------------------------------
four numbers ....

five numbers ......

N numbers ......

...................................................................
in general we could think of a set of outcomes 
1st set combined with 2nd set

---------------------------------------------------------------------

think of the solution being

single number

two numbers one operator

three numbers two operators   ((a ? b) ? c)

four numbers three operators  ((a ? b) ? c ? d )
four numbers three operators  ((a ? b) ? (c ? d))                                  
five numbers four operators   (a ? b ? c ? d ? e )
five numbers four operators   (a ? b ? c ? d ? e )
----------------------------------------------------------------------

think about combine sets , rather thinking about them as individual numbers
1 3 7 10 25 50

think more like 1 being a set - that contains 1 number - ( ( 1  given ) )

we will represent a set as a list , more of a bag then , just a collection of stuff

where given represents given by original problem posed , we can only have same
amount of givens as the problem posed , namely five 

we do not need to use all the given values , we can use only one or two if that
will solve the puzzle

initial sets are
((1 given))
((3 given))
((7 given))
((10 given))
((25 given))
((50 given))
-------------------------------------------------------------------------

or should i work backwards from target to get to 1 ?

target 15
3 5
15 / 3 = 5 stop


------------------------------------------------------------------------
suppose got two independent calculations 

a ? b         c ? d

     used up four numbers only one number left
            e 

(a ? b) ? e         (c ? d)
      then combine 
           or

(a ? b)        (c ? d ? e )
       then combine

--------------------------------------------------------------------------

think of like a stack machine

where target value is to be left on stack

[target]

eg if target is 15 could have stack program " 3 5 * "
> 3 5 *
15
> 5 3 *
15
both these programs solve the original problem " target 15 " 

no operators and result has to be on top of stack with empty stack

a

- two values , if need only one item on stack after computation ,
need one operator only to reduce to a single number

a b ?op

- with three values the stack program is this
a b op c op

- with four values the stack program can be this
a b op c d op op
a b op c op d op
a b c d op op op

the above are different in that 


-------------------------------------------------------------------------

another way think about it is expression tree
an operator + , - , * , /  is a two argument node , 
    op
   / \ 
  a   b


can put op on leaf a , or leaf b 
this gives me these tree below 
     op                op
     /\               /   \ 
   a  op             op    c 
      /\            /  \ 
     b  c          a    b

can put op in a , b or c  - what trees do we get 

---- part A
put an op at a , b or c 

     op          
     /\          
   op   op         
   /\     /\         
  a  d   b  c        

     op          
     /\          
   a  op         
      /\         
     op  c        
     /\
    b  d

     op          
     /\          
   a  op         
      /\         
     b  op        
        /\
       c  d


---- part B
put an op at a , b or c

           op
          /   \ 
         op    c 
        /  \ 
       op    b
       / \
      a   d 

           op
          /   \ 
         op    c 
        /  \ 
       a    op
            /\
           b  d

           op
          /   \ 
         op    op 
        / \    /\
       a   b  c  d

--------------------------------------------------------------------------

so where- ever there is a single value , replace that node with an operator
and put two values on the end

         op               op
         /\               /\ 
        a  b             b  a


|#

;; does build stack space , but then again ,
;; ys is constrained to be 5 elements long at worst


;; (defun combo (x y)
;;   (pick x)
;;   (pick y)
;;   ;; four ways combine with x in first position
;;   (pick '+ x y)
;;   (pick '- x y)
;;   (pick '* x y)
;;   (pick '/ x y)
;;   ;; four ways combine when y in first position
;;   (pick '+ y x)
;;   (pick '- y x)
;;   (pick '* y x)
;;   (pick '/ y x)
;;   )


;; (defun without (x ys)
;;   (cond
;;     ((null? ys) '())
;;     ((= x (car ys)) (cdr ys))
;;     (t (cons (car ys) (without x (cdr ys))))))
;; 
;; 
;; (defun brute (xs)
;;   (dolist (x1 xs)
;;     (let ((w1 (without x1 xs)))
;;       (dolist (x2 w1)
;; 	(let ((w2 (without x2 w1)))
;; 	  (dolist (x3 w2)
;; 	    (let ((w3 (without x3 w2)))
;; 	      (dolist (x w1)
;; 		(let ((w2 (without x2 w1)))
```











