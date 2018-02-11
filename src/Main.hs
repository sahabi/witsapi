{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

type RequestUrl = String

data TradeData =
  TradeData { dataSets  :: !Text
            , lastName   :: !Text
            , age        :: Int
            , likesPizza :: Bool
            } deriving (Show,Generic)

instance FromJSON TradeData
instance ToJSON TradeData

data Dataset = Trade | Tariff | Development
  deriving (Show, Enum)

data Reporter = USA
  deriving (Show, Enum)

data Format = JSON deriving (Show, Enum)

data Request = Request { dataset :: Dataset
                       , reporter :: Reporter
                       , prod :: String
                       , format :: Format
                       , partner :: String
                       , indicator :: String
                       , year :: Int
                       }

years = [1990 .. 2016]
baseUrl = "http://wits.worldbank.org/API/V1/SDMX/V21/datasource/"
indicatorListFile = "/home/sahabi/Downloads/indicatorList.csv"

--  tradestats-trade/reporter/usa/year/ALL/partner/wld/product/ALL/indicator/XPRT-TRD-VL\?format=JSON
getRequestUrl :: Request -> String
getRequestUrl r = baseUrl ++ "tradestats-" ++ show (dataset r) ++ "/reporter/" ++ show (reporter r)
  ++ "/year/" ++ show (year r) ++ "/partner/" ++ partner r
  ++ "/product/" ++ prod r ++ "/indicator/" ++ indicator r ++ "?format="
  ++ show (format r)

-- Read the remote copy of the JSON file.
getJSON :: Request -> IO B.ByteString
getJSON r = simpleHttp url where url = getRequestUrl r

r = Request Trade USA "OresMtls" JSON "ALL" "XPRT-TRD-VL" 2016

main :: IO ()
main = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> getJSON r) :: IO (Either String TradeData)
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right ps -> print ps
