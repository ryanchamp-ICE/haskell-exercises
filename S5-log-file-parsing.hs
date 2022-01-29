-- Original Problem Definition
-- data MessageType = Info
--                   | Warning
--                   | Error Int 
--                   deriving (Show, Eq)

-- type TimeStamp = Int 

-- data LogMessage = LogMessage MessageType TimeStamp String 
--                 | Unknown String 
--                 deriving (Show, Eq)

-- getMessageType :: String -> MessageType

-- getMessageType x:xs = 

-- parseLog :: String -> [LogMessage]
-- parseLog logContents = lines logContents

-- New Problem Definition (Defined in class during session 6)

-- Visual aid
-- logFileLines :: [String]
-- logFileLines =
--     [ "I 147 Vader has risen"
--     , "E 2 148 The droids have invaded the star-ship"
--     ]

-- betterLogFileLines :: [MessageType]
-- betterLogFileLines =
--     [ Information 147 "Vader has risen"
--     , Error 2 148 "The droids have invaded the star-ship"
--     ]

logFile :: String
logFile = "I 147 Vader has risen\nE 2 148 The droids have invaded the star-ship\nW 149 IceColdEdge has entered the building.\nI 150 I.C.E. has started doing work\nW 151 his daughter has come in.\nE 5 152 She knocked over his computer.\nZ23lk4joipfdslkjr\nT 153 Trace message"

data MessageType
    = Information Int String
    | Warning Int String
    | Error Int Int String
    | Unknown String
    deriving (Show, Eq)

parseMessageType :: [String] -> MessageType
parseMessageType [x] = Unknown x
parseMessageType (x:y:z:zs)
  | x == "I" = Information (read y) (unwords [z, unwords zs])
  | x == "W" = Warning (read y) (unwords [z, unwords zs])
  | x == "E" = Error (read y) (read z) (unwords zs)
  | otherwise = Unknown (unwords [x, y, z, unwords zs])

createMessageType :: String -> MessageType
createMessageType line = parseMessageType (words line)

-- parseLogLines :: [String] -> [MessageType]
-- parseLogLines [] = []
-- parseLogLines (x:xs) = createMessageType x : parseLogLines xs
parseLogLines :: [String] -> [MessageType]
parseLogLines lines = map createMessageType lines

parseLog :: String -> [MessageType]
parseLog "" = []
parseLog log = parseLogLines (lines log)

data MessageTree
    = Leaf
    | Node MessageTree MessageType MessageTree
    deriving (Show, Eq)

insert :: MessageType -> MessageTree -> MessageTree
insert msgType Leaf = Node Leaf msgType Leaf
insert (Unknown msgString) msgTree = msgTree
insert input@(Error severity timestamp msgString) curNode@(Node left (Error _ ts _) right)
  | timestamp < ts = insert input left
  | timestamp > ts = insert input right
  | otherwise = curNode
-- insert input@(_ timestamp msgString) curNode@(Node left (Error _ ts _) right)
--   | timestamp < ts = insert input left
--   | timestamp > ts = insert input right
--   | otherwise = curNode

inOrder :: MessageTree -> [MessageType]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

-- whatWentWrong :: [MessageType] -> [String]
-- whatWentWrong msgs = map (inOrder . insert msgs)