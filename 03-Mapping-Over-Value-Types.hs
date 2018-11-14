{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value(..), encode, decode)
import Data.Maybe (fromJust)
import GHC.Exts (fromList)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array x)  = Array (fmap revStrings x)
revStrings (Object x) = let revPair (k, v) = (T.reverse k, revStrings v)
                        in  Object . fromList . map revPair . HM.toList $ x
revStrings other      = other

main = do
    let revJSON = encode . revStrings . fromJust . decode
    T.putStrLn . T.decodeUtf8 . revJSON . T.encodeUtf8 =<< T.getLine
