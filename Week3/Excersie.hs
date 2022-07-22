import Data.List

skips :: [a] -> [[a]]
skips x = go x 1 (length x)
    where go x i l 
            | i == 1 && l /= 0 = x : (go x (i+1) l)
            | i == 2 && i < l  = secondElm x : (go x (i+1) l)
            | i > 2 &&  i < l  = [(x !! i)] : (go x (i+1) l)
            | otherwise        = []

secondElm :: [a] -> [a]
secondElm x = go x (length x) 1
    where go x l i
            | i < l = x !! i : (go x l (i+2))
            | otherwise = []

localMaxima :: [Integer] -> [Integer]
localMaxima x = deleteBy (==) 0 $ go x (length x)
    where
        go x l
            | l >= 3 = (\(a:b:c:_) -> if (b > a && b > c)  then b : (go (drop 2 x )(l-2)) else (go (drop 2 x )(l-2)))(take 3 x) 
            | otherwise = []

localMaxima' :: [Integer] -> [Integer]
localMaxima' (x:rest@(y:z:_))
    | x < y && y > z = y : localMaxima' rest
    | otherwise      = localMaxima' rest
localMaxima' _       = []


replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt i a x 
    | i == 0   = a : x
    |otherwise = (take (i) x) ++ [a] ++ (drop (i+1) x)


histogram :: [Int] -> String
histogram x = concat (map numToStr x) ++ "\n==========\n0123456789\n"
    where
        numToStr a = replaceAt a '*' "\n           \n"

