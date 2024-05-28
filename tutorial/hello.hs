
--- haskell gofer
--- monadic parser in haskell
--- monad is just a function that does other stuff under covers

module Hello where 

--- parser takes a string and returns a parse of a string, with
--- if parse fails - we return [] empty list - no parse possible
--  some videos show option maybe type success | failure 

type Parser a = String -> [(a,String)] 

--- like wrap 
result :: a -> Parser a  
result v = \inp -> [(v,inp)]

--- result 'a'
--- no instance for show parser char

--- zero works
zero :: Parser a
zero = \inp -> []

--- item works 
item :: Parser Char
item = \inp -> case inp of
        [] -> []
        (x : xs) -> [(x ,xs)]


seq :: Parser a -> Parser b -> Parser (a,b)
p `seq` q = \inp -> [((v,w),inp'') | (v,inp') <- p inp , (w,inp'') <- q inp' ]
-- p 'seq' q = \inp -> [((v,w),inp'') |
--   (v,inp') <- q inp ,
--   (w,inp'') <- q inp' ]

--- how make bind an infix operator in haskell ??
--- use backquotes can have a bind operator
bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v,inp') <- p inp ]


--- typical use of bind avoids nested tuples of results
-- p1 ‘bind‘ \x1 ->
-- p2 ‘bind‘ \x2 ->
-- ...
-- pn ‘bind‘ \xn ->
-- result (f x1 x2 ... xn)

--- we can write seq in terms of bind
-- p `seq` q = p `bind` \x ->
--             q `bind` \y ->
--             result (x,y)

            
--- sat takes a predicate that yields a parsre that takes a single
--- character if it satisifes predicate , and fails otherwise
sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` (\x ->  if p x then result x else zero )

--- char matches against single digit, lowercase letters ,
--- uppercase single letters
char :: Char -> Parser Char
char x = sat (\y -> x == y)

--- recognises digits
digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

--- recogniseesl lower case letters
lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

--- recognises upper case letters
upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

--- choice combinator
--- should this not be called OR operator 
plus :: Parser a -> Parser a -> Parser a
p `plus` q = \inp -> (p inp ++ q inp)

--- recognise lower or uppercase alphabet letters as a letter
letter :: Parser Char
letter = lower `plus` upper

--- recognise letter or a digit as alpha numeric character
alphanum :: Parser Char
alphanum = letter `plus` digit

--- word is non deterministic it can take many paths as any sequence of
--- thats why we get many letters of hello in output
word :: Parser String
word = neWord `plus` result "" where
  neWord = letter `bind` \x ->
           word `bind` \xs ->
           result (x:xs)

-- ghci> word "hello peter"
-- [("hello"," peter"),("hell","o peter"),("hel","lo peter"),("he","llo peter"),("h","ello peter"),("","hello peter")]
-- ghci>
---  

-- here are the unit tests 
-- ghci> upper "hello"
-- []
-- ghci> upper "Hello"
-- [('H',"ello")]
-- ghci> lower "Hello"
-- []
-- ghci> lower "hello"
-- [('h',"ello")]
-- ghci> digit "hello"
-- []
-- ghci> digit "0123"
-- [('0',"123")]
-- ghci> 
-- ghci> 
-- test2 = ((lower `bind` (\x ->
--           lower `bind` (\y ->
--      result [x,y] ))) "hello world")  
-- [("he","llo world")]

--- Parser monad
--- result :: a -> Parser a 
--- bind :: Parser a -> (a -> Parser b) -> Parser b
---
---
--- monad is constructor M  a function from types to types
--- result :: a -> M a
--- bind   :: M a -> (a -> M b) -> M b
---

--- 
--- map :: (a -> b) -> (M a -> M b)
--- join :: M ( M a ) -> M a
---

-- class Monad m where
--   result :: a -> m a
--   bind :: m a -> (a -> m b) -> m b
-- 
-- instance Monad Parser where
--   -- result :: a -> Parser a
--   result v = \inp -> [(v,inp)]
--   -- bind :: Parser a -> (a -> Parser b) -> Parser b
--   p `bind` f = \inp -> concat [f v out | (v , out) <- p inp ]

-- class Monad m => MonadOPlus m where
--   zero :: m a
--   (++) :: m a -> m a -> m a
-- 
-- instance MonadOPlus Parser where
--   -- zero :: Parser a
--   zero = \inp -> []
--   -- (++) :: Parser a -> Parser a -> Parser a
--   p ++ q = \inp -> (p inp ++ q inp)

-- special notation for defining parsers of this shape
-- notation can be used with any monad
-- [ f x1 x2 ... xn | x1 <- p1
--                  , x2 <- p2
--                  , ...
--                  , xn <- pn ]


-- without gofer monad comprehensions
-- string :: String -> Parser String 
-- string "" = [""]
-- string (x:xs) = [ x:xs | _ <- char x , _ <- string xs ]

string :: String -> Parser String
string "" = result ""
string (x:xs) = char x `bind` \_ ->
                string xs `bind` \_ ->
                result (x:xs)

-- apply string hello to input hello there                 
-- ghci> ((string "hello") "hello there")
-- [("hello"," there")]
-- ghci> ((string "hello") "helicopter")
-- []




                
                   



            
