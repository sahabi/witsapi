{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Vector as V

import Data.Maybe
import Data.Traversable
import Data.Aeson.Types (Parser, withObject)
import Data.Aeson (FromJSON, ToJSON, Object, Value, parseJSON, eitherDecode)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

newtype TradeData = TradeData { dataSets :: [Object]
                           } deriving (Generic, Show)

instance FromJSON TradeData
instance ToJSON TradeData

data Dataset = Trade | Tariff | Development
  deriving (Show, Enum)

-- better to import the list of reporters due to the large number
data Reporter = USA
  deriving (Show, Enum)

-- add other formats as needed
data Format = JSON deriving (Show, Enum)

-- this is the data structure for the request
data Request = Request { dataset :: Dataset
                       , reporter :: Reporter
                       , prod :: String
                       , format :: Format
                       , partner :: String
                       , indicator :: String
                       , year :: Int
                       } deriving (Show)

-- this function generates the URL from the request
getRequestUrl :: Request -> String
getRequestUrl r = baseUrl ++ "tradestats-" ++ show (dataset r)
  ++ "/reporter/" ++ show (reporter r)
  ++ "/year/" ++ show (year r)
  ++ "/partner/" ++ partner r
  ++ "/product/" ++ prod r
  ++ "/indicator/" ++ indicator r
  ++ "?format=" ++ show (format r)
    where baseUrl = "http://wits.worldbank.org/API/V1/SDMX/V21/datasource/"

-- this function fetches the data given the request parameters
getData :: Request -> IO B.ByteString
getData r = simpleHttp url
  where url = getRequestUrl r

getIndicators = Just ["XPRT-TRD-VL"]
-- Example: Dataset: Trade; Reporter: USA; Product: Ores and Metals; Type: JSON...

newtype Indic = Indic {
  indicatorCode :: String
  } deriving (Show, Generic)

instance C.FromNamedRecord Indic where
    parseNamedRecord r = Indic <$> r C..: "indicatorCode"

getInputList :: (C.FromNamedRecord a) => String -> (a -> String) -> IO [String]
getInputList f p = do
  csvData <- BL.readFile f
  case C.decodeByName csvData of
    Left err -> return []
    Right (_, v) -> return $ foldr ((:) . p) [] v

req :: IO [Request]
req = do
  indList <- getInputList "data/indicatorList.csv" indicatorCode
  return $ Request <$> [Trade ..] <*> [USA] <*> ["OresMtls"] <*> [JSON] <*> ["ALL"] <*>
    indList <*> [2000 .. 2016]

main = do
  rs <- req
  d <- sequence (getData <$> rs) -- :: IO (Either String [TradeData])
  print $ length rs
