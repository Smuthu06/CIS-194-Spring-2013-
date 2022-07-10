{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log 


-- | Parse message 
-- Message structure Code Timestamp info
-- Error message contain the error code also
-- ’I’ for informational messages,
-- ’W’ for warnings, and
-- ’E’ for errors.

parseMessage :: String -> LogMessage
parseMessage str = case words str of
                    "E": c : t : m -> LogMessage (Error (read c :: Int)) (read t :: Int) (unwords m)
                    "I": t : m     -> LogMessage Info (read t :: Int) (unwords m)
                    "W": t : m     -> LogMessage Warnning (read t :: Int) (unwords m)
                    r              -> Unknown $ unwords r

-- | Parse lines into logmessage
parse :: String -> [LogMessage]
parse x = map parseMessage $ lines x

-- # Putting logs in order.

-- greater than all timestamps of any LogMessage in the left subtree.
-- less than all timestamps of any LogMessage in the right child.
-- unknown message should not be stored.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert log Leaf = Node Leaf log Leaf
insert log@(LogMessage _ t1 _) (Node Leaf root@(LogMessage _ t2 _) Leaf) 
    | t1 > t2 = (Node (Node Leaf log Leaf) root Leaf)
    | otherwise = (Node Leaf root (Node Leaf log Leaf))
insert log@(LogMessage _ t1 _) (Node lhs root@(LogMessage _ t2 _) rhs)
    | t1> t2 = (Node (insert log lhs) root rhs)
    | otherwise = (Node lhs root (insert log rhs)) 

-- | Build message tree using list of log messages.
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- | Convert message tree into sorted logmessage.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lhs root rhs) = (inOrder rhs) ++ [root] ++ (inOrder lhs)

-- | Find what went wrong using the sorted logmessages.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = foldr toStr [] lm
  where 
    toStr (Unknown _) i = i
    toStr (LogMessage mt _ msg) i = case mt of
                                      Info -> i
                                      Warnning -> i
                                      (Error code) -> if code > 50 then msg : i else i
                                       
