{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import Data.Aeson.Encoding
import Data.ByteString.Lazy.Char8 as BSL
import Data.Char
import GHC.Generics

data Trend = Trend
  { period :: String
  , africa :: String
  , americas :: String
  , asia :: String
  } deriving Generic

test = Trend {period = "2013", africa = "1", americas = "2", asia = "3"} --, foo = Foo {baz = "whatever"}}

encodeTrend :: Trend -> BSL.ByteString
encodeTrend = encodingToLazyByteString . genericToEncoding defaultOptions

-- Using derived Generics we can cut out all the crap below

-- data Foo = Foo
--   {
--     baz :: String
--   } deriving Generic

-- instance ToJSON Trend where
--   toJSON Trend{..} =
--     object [ "Period"    .= period
--            , "Africa"    .= africa
--            , "Americas"  .= americas
--            , "Asia"      .= asia
--            ]

-- instance ToJSON Trend where
--   toJSON =
--     genericToJSON defaultOptions { fieldLabelModifier = capitaliseFirst }
--       where
--         capitaliseFirst (x:xs) = toUpper x : xs
--         capitaliseFirst []     = []

--   toEncoding = genericToEncoding defaultOptions

--   toEncoding Trend {..} =
--     pairs $ "Period"   .= period
--          <> "Africa"   .= africa
--          <> "Americas" .= americas
--          <> "Asia"     .= asia

-- toTrendEncoding :: Trend -> Encoding
-- toTrendEncoding Trend {..} =
--   pairs $  "Period"   .= period
--         <> "Africa"   .= africa
--         <> "Americas" .= americas
--         <> "Asia"     .= asia
--         <> "Foo"      .= foo

-- toFooEncoding :: Foo -> Encoding
-- toFooEncoding Foo {..} =
--   pairs $ "Baz" .= baz
