module Log where

data MessageType = Info | Warnning | Error Int
                    deriving (Eq, Show)

type Timestamp = Int

data LogMessage = LogMessage MessageType Timestamp String 
                  | Unknown String
                  deriving (Eq, Show)

data MessageTree = Leaf | Node MessageTree LogMessage MessageTree 
                    deriving (Eq, Show)


