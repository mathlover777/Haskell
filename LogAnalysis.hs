import System.Environment
import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file

isTimeStamp::String -> Bool
isTimeStamp _ = True

isErrorLevel::String -> Bool
isErrorLevel _ = True


parseMessage :: [String] -> LogMessage
parseMessage wordList
	|(wordList !! 0) == "I" && isTimeStamp(wordList !! 1) = LogMessage Info (read (wordList!!1)::TimeStamp) (unwords(drop 2 wordList))
	|(wordList !! 0) == "W" && isTimeStamp(wordList !! 1) = LogMessage Info (read (wordList!!1)::TimeStamp) (unwords(drop 2 wordList))
	|(wordList !! 0) == "E" && isTimeStamp(wordList !! 1) && isErrorLevel(wordList !! 2) = LogMessage (Error (read (wordList!!1)::Int)) (read (wordList!!2)::TimeStamp) (unwords(drop 3 wordList))  
	|otherwise = Unknown (unwords(wordList))

parseMessages :: [String]->[LogMessage]
parseMessages [] = []
parseMessages (x:xs) = (parseMessage (words x)):(parseMessages xs)

parse :: String -> [LogMessage]
parse s = parseMessages(lines s)

getTimeStamp::LogMessage -> TimeStamp
getTimeStamp (LogMessage Info a b) = a
getTimeStamp (LogMessage Warning a b) = a
getTimeStamp (LogMessage (Error a) b c) = b
getTimeStamp (Unknown a) = -1

getMessage::LogMessage -> String
getMessage (LogMessage Info a b) = b
getMessage (LogMessage Warning a b) = b
getMessage (LogMessage (Error a) b c) = c
getMessage (Unknown a) = a

getSeverity::LogMessage-> Int
getSeverity (LogMessage (Error a) b c) = a
getSeverity _ = -1

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown x) tree = tree
insert l Leaf = Node Leaf l Leaf
insert l (Node left nodeLog right)
	|getTimeStamp(nodeLog) > getTimeStamp(l) = Node (insert l left) nodeLog right
	|otherwise = Node left nodeLog (insert l right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left nodeLog right) = (inOrder left) ++ [nodeLog] ++ (inOrder right)


verifyInorder::[LogMessage]->[TimeStamp]
verifyInorder [] = []
verifyInorder (x:xs) = getTimeStamp(x):verifyInorder(xs)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (x:xs)
	|getSeverity(x) >= 50 = (getMessage x):whatWentWrong(xs)
	|otherwise = whatWentWrong(xs)

main = do
    --putStrLn "Enter your number : "
    --number <- getLine
    --let n = (read number::Integer)
    --s <- (readFile "error.log")
    s <- (readFile "error.log")
    --print(build(parse s))
    --print(inOrder(build(parse s)))
    --print(verifyInorder( inOrder(build(parse s))))
    print(whatWentWrong (parse s))
   	--testParse parse 10 "error.log"
    --print (unwords(["asdad","adasd"]))
    --createTree(parse s) 
    --print(getTimeStamp(LogMessage Info 1234 "sadad"))
    --print(getTimeStamp(LogMessage Warning 12341 "sadad"))
    --print(getTimeStamp(LogMessage (Error 233) 233 "sadad"))
    --print(getTimeStamp(Unknown "dasddf"))
	--print(insert (LogMessage Info 1234 "sadad") Leaf)