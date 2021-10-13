module Scanning (
    (^&),
    (^*),
    (^|),
    Parseable(fromResult),
    ReadableFromToken(readTok),
    Result,
    Token(..),
    alternating,
    consume,
    end,
    get,
    indicatedBy,
    indicatedByEmptyList,
    grok,
    scanChar,
    scanInt,
    scanStr,
    remember,
    repeating,
    sequential,
    parse1,
    parse2,
    parse3,
    parse4,
    parse5,
    parse6,
    parse7,
) where

import Data.List (isPrefixOf)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Seqs (safeHead)
import MonadUtils (firstOf)
import Debug.Trace(trace)

-- A time for Vocabulary.
-- In this library, I'm working from the following vocab:
-- Nouns:
--  Token: A recognized piece of data.
--  Result:  A collection (often a sequence) of tokens
--  Scanner: A function that turns a string into a result
--
-- Verb:
--  Tokenize: Turn a string into a single token. 
--  Scan: Turn a a string into a result
--  Read: Extract a value from a token
--  Parse: Turn a result into a useful type
--  Grok: Use a specific scanner to comprehend a string and turn it into a useful type.

data Token = Ok
    | IntTok Integer
    | StrTok String
    | CharTok Char
    | RepTok [Result]
    deriving Show

data Result = Failure String | Empty | Remainder String | Scanned Token Result deriving Show

type Scanner = String -> Result

class Scannable a where
    scan :: a -> Scanner

instance Scannable String where
    scan = consume

instance Scannable (Char -> Bool) where
    scan = scanStr 

instance Scannable Scanner where
    scan = id

class Tokenizable a where
    toToken :: a -> Token

instance Tokenizable String where
    toToken s = StrTok s

instance Tokenizable Integer where
    toToken i = IntTok i
 
instance Tokenizable [Result] where
    toToken rs = RepTok rs

instance Tokenizable Char where
    toToken c = CharTok c


(^&) :: (Scannable a, Scannable b) => a -> b -> Scanner
l ^& r = sequential [scan l, scan r]

(^|) :: (Scannable a, Scannable b) => a -> b -> Scanner
l ^|  r = alternating [scan l, scan r]

(^*) :: (Scannable a, Scannable b) => a -> b -> Scanner
target ^* delimiter  = repeating (scan delimiter) (scan target)

consume :: String -> Scanner
consume needle haystack
    | isPrefixOf needle haystack = Scanned Ok $ Remainder $ drop (length needle) haystack
    | otherwise = Failure $ "Could not consume: " ++ needle

remember :: String -> Scanner
remember needle haystack
   | isPrefixOf needle haystack = Scanned (StrTok needle) $ Remainder $ drop (length needle) haystack
   | otherwise = Failure $ "Could not consume: " ++ needle

scanStr :: (Char -> Bool) -> Scanner
scanStr f haystack = Scanned (StrTok left) (Remainder right)
    where (left, right) = span f haystack

intScanner = alternating [sequential [remember "-", scanStr isDigit], scanStr isDigit]

scanInt :: Scanner
scanInt haystack = case (intScanner haystack) of
    Scanned (StrTok m) (Scanned (StrTok d) rest) -> case (readMaybe (m ++ d)) of
        Just n -> Scanned (IntTok n) rest
        Nothing -> Failure ("Not a legal integer: " ++ m ++ d)
    Scanned (StrTok s) rest -> case (readMaybe s) of
        Just n -> Scanned (IntTok n) rest
        Nothing -> Failure ("Not a legal integer: " ++ s)
    other -> other

scanChar :: Scanner
scanChar "" = Failure "Reached end of String"
scanChar (c:rest) = Scanned (CharTok c) $ Remainder rest

sequential :: [Scanner] -> Scanner
sequential [] s = Remainder s
sequential [scanner] s = scanner s
sequential (scanner:rest) s = scanRemaining (scanner s) $ sequential rest

alternating :: [Scanner] -> Scanner
alternating scanners haystack = case success of
        Just r -> r
        _ -> Failure "No Successful Alternations"
    where success = safeHead . (filter isSuccessful) . (map ( $ haystack)) $ scanners

repeating :: Scanner -> Scanner -> Scanner
repeating delimiter target haystack = if isSuccessful head
        then Scanned (RepTok (head:tail)) (Remainder $ firstOf [remainder, toRecurse, toDelimit] haystack)
        else Scanned (RepTok []) (Remainder haystack)
    where (head, toDelimit) = terminate $ target haystack
          (delimited, toRecurse) = case toDelimit of
                Just s -> terminate $ delimiter s
                Nothing -> (Empty, Nothing)
          (tail, remainder) = case repeating delimiter target <$> toRecurse of
                Just (Scanned (RepTok t) (Remainder r)) -> (t, Just r)
                Nothing -> ([], Nothing)

collapse :: Result -> Result
collapse (Scanned Ok rest) = collapse rest
collapse (Scanned (RepTok results) rest) = Scanned (RepTok $ map collapse results) rest
collapse (Scanned tok rest) = Scanned tok $ collapse rest
collapse other = other

indicatedBy :: Tokenizable a => a -> Scanner
indicatedBy tok s = Scanned (toToken tok) $ Remainder s

indicatedByEmptyList :: Scanner
indicatedByEmptyList = indicatedBy ([] :: [Result])

end :: Scanner
end "" = Empty
end s = Failure ("Not at the end: " ++ s)

grok :: Parseable a => Scanner -> String -> Maybe a
grok scanner str = fromResult $ collapse $ scanner str

class ReadableFromToken a where
    readTok :: Token -> Maybe a

class Parseable a where
    fromResult :: Result -> Maybe a

instance ReadableFromToken String where
    readTok (StrTok s) = Just s
    readTok _ = Nothing

instance ReadableFromToken Integer where
    readTok (IntTok i) = Just i
    readTok _ = Nothing

instance ReadableFromToken Char where
    readTok (CharTok c) = Just c
    readTok _ = Nothing

instance {-# OVERLAPPABLE #-} Parseable a => ReadableFromToken [a] where
    readTok (RepTok results) = sequence $ map fromResult results
    readTok _ = Nothing

get :: ReadableFromToken a => Int -> Result -> Maybe a
get _ (Failure _) = Nothing
get _ Empty = Nothing
get _ (Remainder _) = Nothing
get n (Scanned Ok rest) = get n rest
get 0 (Scanned t _) = readTok t
get n (Scanned _ rest) = get (n - 1) rest

parse1 :: ReadableFromToken a => (a -> b) -> Result -> Maybe b
parse1 f r = f <$> (get 0 r)

parse2 :: ReadableFromToken a => ReadableFromToken b => (a -> b -> c) -> Result -> Maybe c
parse2 f r = f <$> (get 0 r) <*> (get 1 r)

parse3 :: ReadableFromToken a => ReadableFromToken b => ReadableFromToken c => (a -> b -> c  -> h) -> Result -> Maybe h
parse3 f r = f <$> (get 0 r) <*> (get 1 r) <*> (get 2 r) 

parse4 :: ReadableFromToken a => ReadableFromToken b => ReadableFromToken c => ReadableFromToken d => (a -> b -> c -> d -> h) -> Result -> Maybe h
parse4 f r = f <$> (get 0 r) <*> (get 1 r) <*> (get 2 r) <*> (get 3 r)

parse5 :: ReadableFromToken a => ReadableFromToken b => ReadableFromToken c => ReadableFromToken d => ReadableFromToken e => (a -> b -> c -> d -> e -> h) -> Result -> Maybe h
parse5 f r = f <$> (get 0 r) <*> (get 1 r) <*> (get 2 r) <*> (get 3 r) <*> (get 4 r)

parse6 :: ReadableFromToken a => ReadableFromToken b => ReadableFromToken c => ReadableFromToken d => ReadableFromToken e => ReadableFromToken f => (a -> b -> c -> d -> e -> f -> h) -> Result -> Maybe h
parse6 f r = f <$> (get 0 r) <*> (get 1 r) <*> (get 2 r) <*> (get 3 r) <*> (get 4 r) <*> (get 5 r)
parse7 :: ReadableFromToken a => ReadableFromToken b => ReadableFromToken c => ReadableFromToken d => ReadableFromToken e => ReadableFromToken f => ReadableFromToken g => (a -> b -> c -> d -> e -> f -> g -> h) -> Result -> Maybe h
parse7 f r = f <$> (get 0 r) <*> (get 1 r) <*> (get 2 r) <*> (get 3 r) <*> (get 4 r) <*> (get 5 r) <*> (get 6 r)


------------
-- Utilities:
-------------
terminate :: Result -> (Result, Maybe String)
terminate (Failure s) = (Failure s, Nothing)
terminate Empty = (Empty, Nothing)
terminate (Remainder s) = (Empty, Just s)
terminate (Scanned tok rest) = (Scanned tok rest', remainder)
    where (rest', remainder) = terminate rest

getRemainder :: Result -> Maybe String
getRemainder (Remainder s) = Just s
getRemainder (Scanned _ rest) = getRemainder rest
getRemainder _ = Nothing

scanRemaining :: Result -> Scanner -> Result
scanRemaining (Remainder s) scanner = scanner s
scanRemaining (Scanned tok res) scanner = Scanned tok (scanRemaining res scanner)
scanRemaining Empty _ = Failure "Nothing left to Scan"
scanRemaining x _ = x -- Failure, chiefly.

isSuccessful :: Result -> Bool
isSuccessful (Scanned _ rest) = isSuccessful rest
isSuccessful (Failure _) = False
isSuccessful _ = True
