{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Aeson                     ( Value (..)
                                                , encode
                                                , decode
                                                , eitherDecode
                                                , object
                                                , (.=)
                                                )
import           Data.Aeson.Types               ( Parser, parseMaybe )
import           Data.Maybe                     ( fromJust )
import           GHC.Exts                       ( fromList )

import qualified Data.ByteString.Lazy.Char8    as BS
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.IO             as T
import qualified Data.Text.Lazy.Encoding       as T

-------------------------------------------------------------------------------

basicEncodeDecode01 :: IO ()
basicEncodeDecode01 = do
  putStr2Lns (decode "[1,2,3]" :: Maybe [Integer])
  putStr2Lns (decode "foo" :: Maybe [Integer])
  putStr2Lns (eitherDecode "\"foo\"" :: Either String String)
  putStr2Lns (eitherDecode "\"foo\"" :: Either String Integer)
  putStr2Lns (eitherDecode "foo" :: Either String String)
  putStr2Lns
    (eitherDecode "[1,2,[3,true]]" :: Either String (Int, Int, (Int, Bool)))
  putStr2Lns
    (eitherDecode "[1,2,[3,4]]" :: Either String (Int, Int, (Int, Bool)))
 where
  putStr2Lns :: Show a => a -> IO ()
  putStr2Lns = putStrLn . (++ "\n") . show

-------------------------------------------------------------------------------

val :: Value
val = Object $ fromList
  [ ("numbers", Array $ fromList [Number 1, Number 2, Number 3])
  , ("boolean", Bool True)
  ]

buildValueAST02 :: IO ()
buildValueAST02 = T.putStrLn . T.decodeUtf8 . encode $ val

-------------------------------------------------------------------------------

val2 :: Value
val2 = object ["boolean" .= True, "numbers" .= [1, 2, 3 :: Int]]
                                    -- a type annotation is needed because
                                    -- otherwise it's unclear whether it should
                                    -- be Int or, say, Double or Rational

buildValueAST03 :: IO ()
buildValueAST03 = T.putStrLn . T.decodeUtf8 . encode $ val2

-------------------------------------------------------------------------------

revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array  x) = Array (fmap revStrings x)
revStrings (Object x) =
  let revPair (k, v) = (T.reverse k, revStrings v)
  in  Object . fromList . map revPair . HM.toList $ x
revStrings other = other

reverseStringFromGetLine04 :: IO ()
reverseStringFromGetLine04 = do
  let revJSON = encode . revStrings . fromJust . decode
  T.putStrLn . T.decodeUtf8 . revJSON . T.encodeUtf8 =<< T.getLine

-------------------------------------------------------------------------------

-- Aeson has a type called Parser, but it's not a type for parsers
--   – it's a type for results of parsers

-- 'Parser a' means pretty much the same as 'Either String a'

parseTuple :: Value -> Parser (String, Bool)
parseTuple (Object obj) = do
  -- Look up the "a" field.
  let mbFieldA = HM.lookup "a" obj

  -- Fail if it wasn't found.
  fieldA <- case mbFieldA of
    Just x  -> return x
    Nothing -> fail "no field 'a'"

  -- Extract the value from it, or fail if it's of the wrong type.
  a <- case fieldA of
    String x -> return (T.unpack x)
    _        -> fail "expected a string"

  -- Do all the same for "b" (in a slightly terser way, to save space):
  b <- case HM.lookup "b" obj of
    Just (Bool x) -> return x
    Just _        -> fail "expected a boolean"
    Nothing       -> fail "no field 'b'"

  -- That's all!
  return (a, b)
parseTuple _ = fail "expected an object"

simpleParser05 :: IO ()
simpleParser05 = do
  let jsonString = "{\"a\":\"foo\", \"b\": false}"
  putStrLn "Parser with valid input"
  BS.putStrLn jsonString
  print $ parseMaybe parseTuple =<< decode jsonString
  putStrLn "\nParser with invalid input:"
  let jsonStringInv = "{\"foo\":\"bar\", \"b\": false}"
  BS.putStrLn jsonStringInv
  print $ parseMaybe parseTuple =<< decode jsonStringInv

-------------------------------------------------------------------------------

-- Set this to determine which examples run
toRun :: [Int]
toRun = [5]

-------------------------------------------------------------------------------

main :: IO ()
main = do
  when (1 `elem` toRun) $ do
    basicEncodeDecode01
    putStrLn "-------------------------------"
  when (2 `elem` toRun) $ do
    buildValueAST02
    putStrLn "-------------------------------"
  when (3 `elem` toRun) $ do
    buildValueAST03
    putStrLn "-------------------------------"
  when (4 `elem` toRun) $ do
    reverseStringFromGetLine04
    putStrLn "-------------------------------"
  when (5 `elem` toRun) $ do
    simpleParser05
    putStrLn "-------------------------------"

