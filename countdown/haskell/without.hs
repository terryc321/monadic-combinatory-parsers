
-- without : Int -> [Int] -> [Int]

without x [] = []
without x (a : xs) = if x == a then xs else a : (without x xs)
---without x (a : xs) = a : (without x xs)




  

