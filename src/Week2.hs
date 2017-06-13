{-# OPTIONS_GHC -Wall #-}
module Week2 where

import Log
import Data.Char
import Prelude


parse:: String -> [LogMessage]
parse s = map parseMessage (lines s)

parseMessage :: String -> LogMessage
parseMessage s = do
    let (mType, s1) = parseMessageType s
    let (timestamp, s2) = getDigitUntilSpace s1
    LogMessage mType timestamp s2

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . severeMessagesInOrder
-- whatWentWrong messages = map getMessage $ severeMessagesInOrder messages

whoDunit :: [LogMessage] -> [String]
whoDunit messages = do
    let (LogMessage _ time _) = head $ severeMessagesInOrder messages
    map getMessage . inOrder . build $ getMessagesBeforeTime messages time

getMessagesBeforeTime :: [LogMessage] -> TimeStamp -> [LogMessage]
getMessagesBeforeTime messages latestTime = filter (\(LogMessage _ time _) -> time < latestTime) messages

severeMessagesInOrder :: [LogMessage] -> [LogMessage]
severeMessagesInOrder messages = inOrder . build $ filter isSevereMessage messages

isSevereMessage :: LogMessage -> Bool
isSevereMessage (LogMessage (Error sev) _ _)
    | sev >= 50 = True
isSevereMessage _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message ) = message

inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node left message right) = do
    let leftMessages = inOrder left
    let rightMessages = inOrder right
    leftMessages ++ [message] ++ rightMessages

build :: [LogMessage] -> MessageTree
build messages = buildHelper messages Leaf

buildHelper :: [LogMessage] -> MessageTree -> MessageTree
buildHelper (x:xs) tree = buildHelper xs $ insert x tree
buildHelper ([]) tree = tree

insert:: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message (Leaf) = Node Leaf message Leaf
insert message @ (LogMessage _ addTime _ ) (Node left curMessage @ (LogMessage _ timestamp _) right)
    | addTime > timestamp = Node left curMessage (insert message right)
    | otherwise = Node (insert message left) curMessage right

parseMessageType :: String -> (MessageType,String)
parseMessageType ('E':' ':xs) = do
    let (errorCode, s) = getDigitUntilSpace xs
    (Error errorCode, s)
parseMessageType ('W':' ':xs) = (Warning, xs)
parseMessageType ('I':' ':xs) = (Info, xs)
parseMessageType (_:xs) = (Error 0, xs)
parseMessageType ([]) = (Error 0, "")

getDigitUntilSpace :: String -> (Int, String)
getDigitUntilSpace s = getDigitUntilSpaceHelper s ""

getDigitUntilSpaceHelper :: String -> String -> (Int, String)
getDigitUntilSpaceHelper ([]) s = ((read s :: Int), s)
getDigitUntilSpaceHelper (' ':xs) s = ((read s :: Int), xs)
getDigitUntilSpaceHelper (x:xs) s = getDigitUntilSpaceHelper xs (s ++ [x])
