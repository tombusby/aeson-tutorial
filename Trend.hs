{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson

data Trend = Trend
            { period :: String
            , africa :: String
            , americas :: String
            , asia :: String
            } deriving Show

instance ToJSON Trend where
  toJSON Trend{..} =
    object [ "Period"    .= period
           , "Africa"    .= africa
           , "Americas"  .= americas
           , "Asia"      .= asia
           ]

  toEncoding Trend {..} =
    pairs $ "Period"   .= period
         <> "Africa"   .= africa
         <> "Americas" .= americas
         <> "Asia"     .= asia

test = Trend {period = "2013", africa = "1", americas = "2", asia = "3"}
