{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Csv ((.:), FromNamedRecord, parseNamedRecord, decodeByName)
import Data.Either
import Data.Maybe
import Data.Traversable
import Data.Aeson.Types (Parser, withObject)
import Data.Aeson (FromJSON, ToJSON, Object, Value, parseJSON, eitherDecode, decode)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import Control.Monad
import Control.Exception
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

newtype TradeData = TradeData { dataSets :: [Object]
                           } deriving (Generic, Show)

instance FromJSON TradeData
instance ToJSON TradeData

data Dataset = Trade | Tariff | Development
  deriving (Show, Enum)

-- add other formats as needed
data Format = JSON deriving (Show, Enum)

-- this is the data structure for the request
data Request = Request { dataset :: Dataset
                       , reporter :: String
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
getData :: Request -> IO (Maybe B.ByteString)
getData r = handle errorHandler $ Just <$> simpleHttp url
  where url = getRequestUrl r
        errorHandler :: SomeException -> IO (Maybe B.ByteString)
        errorHandler _ = return Nothing

newtype Reporter = Reporter {
  reporterCode :: String
  } deriving (Show, Generic)

instance FromNamedRecord Reporter where
    parseNamedRecord r = Reporter <$> r .: "reporterCode"

newtype Indicator = Indicator {
  indicatorCode :: String
  } deriving (Show, Generic)

instance FromNamedRecord Indicator where
    parseNamedRecord r = Indicator <$> r .: "indicatorCode"

getInputList :: (FromNamedRecord a) => String -> (a -> b) -> IO [b]
getInputList f p = do
  csvData <- BL.readFile f
  case decodeByName csvData of
    Left err -> return []
    Right (_, v) -> return $ foldr ((:) . p) [] v

getIndicatorList = getInputList "data/indicatorList.csv" indicatorCode
getCountryList =  getInputList "data/CountryList.csv" reporterCode

req :: IO [Request]
req = do
  indList <- getIndicatorList
  countryList <- getCountryList
  return $ Request <$> [Trade ..] <*> countryList <*> ["OresMtls"] <*> [JSON] <*> ["ALL"] <*>
    take 10 indList <*> [2016]

main = do
  rs <- req
  d <- fmap (maybe (Just (TradeData [])) decode) <$> sequence (getData <$>  rs) :: IO [Maybe TradeData]
  print d
  print $ length rs
