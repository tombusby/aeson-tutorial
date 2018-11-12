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
    where
        putStr2Lns :: Show a => a -> IO ()
        putStr2Lns = putStrLn . (++"\n") . show
