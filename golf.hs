import System.Environment

takeOnlyNth::Int -> [a] ->[a]
takeOnlyNth _ [] = []
takeOnlyNth n l
	|n<=0 = []
	|length(l) <n = []
	|otherwise = (l!!(n-1)):(takeOnlyNth n (drop n l))

skipList::Int->[a]->[[a]]
skipList size l
	|size == length(l) = [[last l]]
	|otherwise = (takeOnlyNth size l):(skipList (size+1) l)

skips :: [a] -> [[a]]
skips [] = []
skips l = skipList 1 l

localMaxima :: [Integer] -> [Integer]
localMaxima l
	|length(l)<3 = []
localMaxima (x:y:z:xs)
	|(y>z && y>x) = y:(localMaxima (y:z:xs))
	|otherwise = (localMaxima (y:z:xs))

count::Integer->[Integer]->Integer
count n [] = 0
count n (a:xs)
	|n == a = 1 + (count n xs)
	|otherwise = (count n xs)   

countArrayInternal::Integer->[Integer]->[Integer]
countArrayInternal n x
	|n < 0 = []
	|otherwise = (count n x):(countArrayInternal (n-1) x)

countArray::[Integer]->[Integer]
countArray l = reverse (countArrayInternal 9 l)

getMax::[Integer]->Integer
getMax [] = 0
getMax [a] = a
getMax (x:xs) = max x (getMax xs)

getStarsOfLevel::Integer->[Integer]->String
getStarsOfLevel 0 x = ""
getStarsOfLevel l [] = ""
getStarsOfLevel l (x:xs)
	|l <= x = "*"++(getStarsOfLevel l xs)
	|otherwise = " "++(getStarsOfLevel l xs)

getStarsOfLevelWithMax::Integer->[Integer]->String
getStarsOfLevelWithMax n l
	|n>0 = (getStarsOfLevel n l)++"\n"++(getStarsOfLevelWithMax (n-1) l)
	|otherwise = ""


getStars::[Integer]->String
getStars l = (getStarsOfLevelWithMax (getMax l) l) 



getLabel::String
getLabel = "==========\n0123456789"

histogram :: [Integer] -> String
histogram l = (getStars (countArray l))++getLabel

main = do
    --putStrLn "Enter your number : "
    --number <- getLine
    --let n = (read number::Integer)
    --print(takeOnlyNth 2 [1,2,3,4,5,6,7])
    --print(takeOnlyNth 0 [1,2,3,4,5,6,7])
    --print(takeOnlyNth 2 [1])
    ----print(takeOnlyNth 2 [])
    --print(takeOnlyNth 3 [1,2,3,4,5,6,7])
    --print(takeOnlyNth 3 [1,2,3,4,5,6,7,8])
    --print(takeOnlyNth 3 [1,2,3,4,5,6,7,8,9])
    --print(skips "ABCD")
	--print(skips "hello!")
	--print(skips [1])
	--print(skips [True,False])
	--print([])
	--print(localMaxima [2,9,5,6,1])
	--print(localMaxima [2,3,4,1,5])
	--print(localMaxima [1,2,3,4,5])
	--putStrLn(getLabel)
	--print(getMax [1,4,5,4,6,6,3,4,2,4])
	--print(countArray [1,4,5,4,6,6,3,4,2,4,9])
	putStrLn(histogram [1,4,5,4,6,6,3,4,2,4,9])