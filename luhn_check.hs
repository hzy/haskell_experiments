toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0    = []
    | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : y * 2 : doubleEveryOther xs

splitDigits :: Integer -> [Integer]
splitDigits x
    | x < 10    = [x]
    | otherwise = (x `mod` 10) : (splitDigits (x `div` 10))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (splitDigits x)) + (sumDigits xs)

validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigitsRev x))) `mod` 10 == 0

main = do
    print (validate 4012888888881881)
    print (validate 4012888888881882)
