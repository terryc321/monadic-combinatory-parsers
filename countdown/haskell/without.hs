
module Main where


without :: Eq t => t -> [t] -> [t]

-- without : Int -> [Int] -> [Int]
without x [] = []
without x (a : xs) = if x == a then xs else a : (without x xs)
---without x (a : xs) = a : (without x xs)


--- take 2 items from some list and some ordering also
take2f f [] = []
take2f f ( _ : [] ) = []
take2f f xs = map f [ (x , y , without y (without x xs)) | x <- xs , y <- without x xs , x < y ]

myadd (x,y,zs) = (x + y) : zs 
mymul (x,y,zs) = if x == 1 then [] else if y == 1 then [] else (x * y) : zs 
mydiv (x,y,zs) = if x == 1 then [] else if y == 1 then [] else if mod y x == 0 then div y x : zs else [] 
mysub (x,y,zs) = if x == y then [] else (y - x) : zs 

mystep xs = (filter (\x -> (not (null x)))
             ((take2f mymul xs) ++
              (take2f myadd xs) ++
              (take2f mysub xs) ++
              (take2f mydiv xs)
             ))
            
--- cps the out of it ?

--get1 :: IO ()
get1 [] = do putStrLn ""
get1 (x : xs) =  do putStr "I got "
                    putStrLn $ (show x)
                    get1 xs


-- get2 a [] cont = []
-- get2 a (x : xs) cont = do cont (a,x,xs)
--                           get2 a xs cont


                      
                        
puzzle = [1,3,7,10,25,50]
target = 765

main :: IO ()
main = do get1 puzzle

---pick1 :: [Int] -> Maybe (Int , [Int])
-- pick1 (x : xs) = Just (x,xs) 


--- surprised myself this actually passed the type checker ,,,,,,
--- does not quite do what need
--- also returns a cons ?
pick1 [] = []
pick1 (x : xs) = pick2 x xs (\y -> pick1 xs)

pick2 x [] cont = cont []
pick2 x (y : ys) cont = pick3 x y ys (\z -> pick2 x ys cont)

pick3 x y z cont = (x,y,z) : (cont [])


--- need is a way to do this in cps lambda free type checker then figure out if we can
--- get the types to align
--- what is the type of continuation ?
--- for that need to know the rest of the computation surely ? 
--- will change , each 

--- lets give pick1 another arg to store stuff in if x is not picked 
pick1' [] rs = rs
pick1' (x : xs) rs = pick2' x xs (\y -> pick1' xs rs)

pick xs = pick1' xs []

pick2' x [] cont = cont []
pick2' x (y : ys) cont = pick3' x y ys (\z -> pick2' x ys cont)

pick3' x y z cont = (x,y,z) : (cont [])






  

