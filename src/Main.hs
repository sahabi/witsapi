{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as B
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP (withHTTP)
import Text.XML.HXT.Curl (withCurl)
import Control.Applicative
import Control.Monad

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

getTariffRequestUrl :: Tariff -> String
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

getParam t1 t2 con = atTag t1 >>>
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
getCountryList = getParamList countryCode (metaBaseUrl ++ "/country/all") getCountry
  where getCountry = getParam "wits:country" "wits:iso3Code" Country

getIndicatorList :: IO [String]
getIndicatorList = getParamList indicatorCode (metaBaseUrl ++ "/indicator/all") getIndicator
  where getIndicator = getParam "wits:indicator" "indicatorcode" Indicator

getProductList :: IO [String]
getProductList = getParamList productCode (metaBaseUrl ++ "/product/all") getProduct
  where getProduct = getParam "wits:product" "productcode" Product

getObs :: String -> IO [String]
getObs url = getParamList observation url getObservation
  where getObservation = getParam "Obs" "OBS_VALUE" Observation

getTariffs :: String -> IO [Tariff]
getTariffs = undefined

mkRequests :: IO [Tariff]
mkRequests = do
  indList <- getIndicatorList
  countryList <- getCountryList
  productList <- getProductList
  return $ Tariff <$> ["usa"]
                  <*> [2010]
                  <*> ["all"]
                  <*> ["all"]
                  <*> indList
                  <*> [""]

metaBaseUrl = "http://wits.worldbank.org/API/V1/wits/datasource/tradestats-tariff"
dataBaseUrl = "http://wits.worldbank.org/API/V1/SDMX/V21/datasource/tradestats-tariff"

main = do
  req <- mkRequests
  let urls = getTariffRequestUrl <$> req
  ts <- sequence $ getTariffs <$> urls
  sequence $ print <$> ts
