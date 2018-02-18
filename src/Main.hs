{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as B
import qualified Network.HTTP as H
import Data.Maybe
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP (withHTTP)
import Text.XML.HXT.Curl (withCurl)
import Control.Applicative
import Control.Monad
import Text.Pretty.Simple (pPrint)

newtype Country = Country { countryCode :: String } deriving (Show, Eq)
newtype Indicator = Indicator { indicatorCode :: String } deriving (Show, Eq)
newtype Product = Product { productCode :: String } deriving (Show, Eq)
newtype Observation = Observation { observation :: String } deriving (Show, Eq)

data Tariff = Tariff { country :: String
                     , year :: Int
                     , partner :: String
                     , prod :: String
                     , indicator :: String
                     , obs :: String
                     } deriving (Show)

type Request = Tariff
type URL = String

getTariffRequestUrl :: Request -> URL
getTariffRequestUrl r = dataBaseUrl
  ++ "/reporter/" ++ country r
  ++ "/year/" ++ show (year r)
  ++ "/partner/" ++ partner r
  ++ "/product/" ++ prod r
  ++ "/indicator/" ++ indicator r

parseXML = readDocument [ withValidate no
                        , withRemoveWS yes
                        , withHTTP []
                        , withCurl []
                        ]

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

parseParam t1 t2 con = atTag t1 >>>
  proc x ->
    case take 4 t2 of
      "wits" -> do
        param <- text <<< atTag t2 -< x
        returnA -< con param
      _ -> do
        param <- getAttrValue t2 -< x
        returnA -< con param

getParamList f1 url f2 = fmap f1 <$> runX (parseXML url  >>> f2)

getCountryList :: IO [String]
getCountryList = getParamList countryCode (metaBaseUrl ++ "/country/all") parseCountry
  where parseCountry = parseParam "wits:country" "wits:iso3Code" Country

getIndicatorList :: IO [String]
getIndicatorList = getParamList indicatorCode (metaBaseUrl ++ "/indicator/all") parseIndicator
  where parseIndicator = parseParam "wits:indicator" "indicatorcode" Indicator

getProductList :: IO [String]
getProductList = getParamList productCode (metaBaseUrl ++ "/product/all") parseProduct
  where parseProduct = parseParam "wits:product" "productcode" Product

getObs :: String -> IO [String]
getObs url = getParamList observation url parseObservation
  where parseObservation = parseParam "Obs" "OBS_VALUE" Observation

parseTariffs = atTag "Series" >>>
  proc x -> do
    r <- getAttrValue "REPORTER" -< x
    p <- getAttrValue "PARTNER" -< x
    prod <- getAttrValue "PRODUCTCODE" -< x
    part <- getAttrValue "PARTNER" -< x
    ind <- getAttrValue "INDICATOR" -< x
    obs <- atTag "Obs" -< x
    y <- getAttrValue "TIME_PERIOD" -< obs
    obsv <- getAttrValue "OBS_VALUE" -< obs
    returnA -< Tariff r (read y :: Int) part prod ind obsv

mkRequests :: IO [Request]
mkRequests = do
  indList <- getIndicatorList
  countryList <- getCountryList
  productList <- getProductList
  return $ Tariff <$> countryList
                  <*> [1990 .. 2017]
                  <*> ["all"]
                  <*> ["all"]
                  <*> indList
                  <*> [""]

metaBaseUrl = "http://wits.worldbank.org/API/V1/wits/datasource/tradestats-tariff"
dataBaseUrl = "http://wits.worldbank.org/API/V1/SDMX/V21/datasource/tradestats-tariff"

getTariffs url = runX (parseXML url >>> parseTariffs)

main :: IO ()
main = do
  rq <- mkRequests
  let urls = getTariffRequestUrl <$> rq
  tar <- sequence $ getTariffs <$> urls
  print $ length $ concat tar
