module Week2.LogAnalysis  where

import Week2.Log

type MsgBuilder = TimeStamp -> String -> LogMessage

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . inOrder . build . filter seriousError
  where
    seriousError (LogMessage (Error c) _ _) | c >= 50 = True
    seriousError _ = False
    getMsg (LogMessage _ _ msg) = msg
    getMsg (Unknown _) = error "Message de type inconnu"

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 m t2) = inOrder t1 ++ [m] ++ inOrder t2

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert _ (Node _ (Unknown _) _) = error "L'arbre contient un message de type inconnu"
insert m @ (LogMessage _ tsi _) (Node t1 lm @ (LogMessage _ ts _) t2)
  | tsi <= ts = Node (insert m t1) lm t2
  |  otherwise = Node t1 lm (insert m t2)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":err:tstamp:msg) ->
    LogMessage (Error $ parseInt err) (parseInt tstamp) (unwords msg)
  (mtype:tstamp:msg) ->
    parseBenignMsg mtype (parseInt tstamp) (unwords msg)
  _ -> Unknown s

parseBenignMsg :: String -> MsgBuilder
parseBenignMsg t
  | t == "I" = LogMessage Info
  | t == "W" = LogMessage Warning
  | otherwise = unknown

unknown :: MsgBuilder
unknown _ = Unknown

parseInt :: String -> Int
parseInt = read
