{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson

main = do
    putStrLn "decode \"[1,2,3]\" :: Maybe [Integer]"
    putStr2Lns (decode "[1,2,3]" :: Maybe [Integer])
    putStrLn "decode \"foo\" :: Maybe [Integer]"
    putStr2Lns (decode "foo" :: Maybe [Integer])
    putStrLn "eitherDecode \"\\\"foo\\\"\" :: Either String String"
    putStr2Lns (eitherDecode "\"foo\"" :: Either String String)
    putStrLn "eitherDecode \"\\\"foo\\\"\" :: Either String Integer"
    putStr2Lns (eitherDecode "\"foo\"" :: Either String Integer)
    putStrLn "eitherDecode \"foo\" :: Either String String"
    putStr2Lns (eitherDecode "foo" :: Either String String)
    putStrLn "eitherDecode \"[1,2,[3,true]]\" :: Either String (Int, Int, (Int, Bool))"
    putStr2Lns (eitherDecode "[1,2,[3,true]]" :: Either String (Int, Int, (Int, Bool)))
    putStrLn "eitherDecode \"[1,2,[3,4]]\" :: Either String (Int, Int, (Int, Bool))"
    putStr2Lns (eitherDecode "[1,2,[3,4]]" :: Either String (Int, Int, (Int, Bool)))
    where
        putStr2Lns :: Show a => a -> IO ()
        putStr2Lns = putStrLn . (++"\n") . show
