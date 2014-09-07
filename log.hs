import System.Environment

data MessageType = Info
	| Warning
	| Error Int
	deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
	| Unknown String
	deriving (Show, Eq)

main = do
    putStrLn "Enter your number : "
    number <- getLine
    let n = (read number::Integer)
    print(n)