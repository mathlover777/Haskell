import System.Environment

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n a b c
	|n<=0 = [("No","Move")]
	|n==1 = [(a,b)]
	|otherwise = ((hanoi (n-1) a c b)++[(a,b)])++(hanoi (n-1) c b a)

main = do
    putStrLn "Enter your number : "
    number <- getLine
    let n = (read number::Integer)
    print(hanoi n "a" "b" "c")