-- | validate the credit/debit card using checksum

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0    = []
    | otherwise = (mod n 10) : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits = reverse.toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = go x True
    where go [] _ = []
          go (x:xs) b
            | b         = (x*2) : go xs (not b)
            | otherwise = x : go xs (not b)

sumDigits :: [Integer] -> Integer
sumDigits x = sum $ map (sum . toDigits) x

-- >>> validate 4012888888881881
-- >>> validate 4012888888881882
-- True
-- False
validate :: Integer -> Bool
validate n = mod (sumDigits . doubleEveryOther . toDigits $ n) 10  == 0

-- | Tower of Hannoi

type Peg = String
type Move = (Peg, Peg)

-- >>> hanoi 3 "A" "B" "C"
-- [("A","B"),("A","C"),("B","C"),("A","B"),("C","A"),("C","B"),("A","B")]
--- >>> hanoi 2 "A" "B" "C"
-- [("A","C"),("A","B"),("C","B")]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _          = []
hanoi 1 start end _    = [(start, end)]
hanoi 2 start end temp = [(start, temp), (start, end), (temp, end)]
-- if the number of ring more than 2 we will recurse the hanoi to find the move.
hanoi n start end temp =
    let nMinusOne = subtract 1 n
    in hanoi nMinusOne start temp end ++
       hanoi 1 start end temp ++
       hanoi nMinusOne temp end start




