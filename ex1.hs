import System.Environment

toDigitsReversed :: Integer -> [Integer]
toDigitsReversed n
	| (n<0) = [0]
	| (n>=0 && n<10) = [n]
	| otherwise = (n `mod` 10):(toDigitsReversed ((n-(n `mod` 10))`div`10))

removeLast::[Integer] -> [Integer]
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x:(removeLast xs)


reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList [x] = [x]
reverseList xs = (last xs):(reverseList(removeLast(xs)))

toDigits::Integer -> [Integer]
toDigits n = reverseList (toDigitsReversed n)

doubleEveryOtherFromLeft::[Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft [x,y] = [x,2*y]
doubleEveryOtherFromLeft (x:(y:xs)) = x:((2*y):doubleEveryOtherFromLeft(xs))

doubleEveryOtherFromRight::[Integer]->[Integer]
doubleEveryOtherFromRight n = reverseList (doubleEveryOtherFromLeft (reverseList n))

sumDigitsOfNumber::Integer->Integer
sumDigitsOfNumber n
	|n<0 = 0
	|n>=0 && n<10 =n
	|otherwise = (mod n 10) + sumDigitsOfNumber(div n 10)

sumDigitsOfArray::[Integer]->Integer
sumDigitsOfArray [] = 0
sumDigitsOfArray [x] = sumDigitsOfNumber(x)
sumDigitsOfArray (x:xs) = sumDigitsOfNumber(x) + sumDigitsOfArray(xs)

isDivisibleBy10::Integer->Bool
isDivisibleBy10 n
	|(mod n 10) == 0 = True
	|otherwise = False

validate::Integer->Bool
validate n = isDivisibleBy10(sumDigitsOfArray(doubleEveryOtherFromRight(toDigits n)))

main = do
    putStrLn "Enter your number : "
    number <- getLine
    let n = (read number::Integer)
    print(validate n)
