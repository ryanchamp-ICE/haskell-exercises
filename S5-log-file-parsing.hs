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

-- logFile :: String
-- logFile = "I 147 Vader has risen\nE 2 148 The droids have invaded the star-ship\nW 149 IceColdEdge has entered the building.\nI 150 I.C.E. has started doing work\nW 151 his daughter has come in.\nE 5 152 She knocked over his computer.\nZ23lk4joipfdslkjr\nT 153 Trace message"

logFile :: String
logFile = "I 147 Vader has risen\nE 51 152 She knocked over his computer.\nI 150 I.C.E. has started doing work\nW 151 his daughter has come in.\nW 149 IceColdEdge has entered the building.\nZ23lk4joipfdslkjr\nT 153 Trace message\nE 2 148 The droids have invaded the star-ship\n"

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

getTimeStamp :: MessageType -> Int 
getTimeStamp (Information ts _) = ts
getTimeStamp (Warning ts _) = ts
getTimeStamp (Error _ ts _) = ts
getTimeStamp (Unknown _) = -999

getSeverity :: MessageType -> Int
getSeverity (Information _ _) = 0
getSeverity (Warning _ _) = 0
getSeverity (Error sev _ _) = sev
getSeverity (Unknown _) = 0

getMessageString :: MessageType -> String
getMessageString (Information _ msgString) = msgString
getMessageString (Warning _ msgString) = msgString
getMessageString (Error _ _ msgString) = msgString
getMessageString (Unknown msgString) = msgString

insert :: MessageType -> MessageTree -> MessageTree
insert msgType Leaf = Node Leaf msgType Leaf
insert (Unknown msgString) msgTree = msgTree
insert input curNode@(Node left nodeMsgType right)
  | getTimeStamp input < getTimeStamp nodeMsgType = Node (insert input left) nodeMsgType right 
  | getTimeStamp input > getTimeStamp nodeMsgType = Node left nodeMsgType (insert input right)
  | otherwise = curNode

insertListIntoTree :: [MessageType] -> MessageTree
insertListIntoTree [] = Leaf
insertListIntoTree [x] = insert x Leaf
insertListIntoTree (x:xs) = insert x (insertListIntoTree xs)

inOrder :: MessageTree -> [MessageType]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

filterCatastrophes :: [MessageType] -> [MessageType]
filterCatastrophes [] = []
filterCatastrophes (x:xs)
  | getSeverity x > 50 = x : filterCatastrophes xs
  | otherwise = filterCatastrophes xs

whatWentWrong :: [MessageType] -> [String]
whatWentWrong msgs = map getMessageString (filterCatastrophes (inOrder (insertListIntoTree msgs)))