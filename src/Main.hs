import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

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
                       }

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

-- Example: Dataset: Trade; Reporter: USA; Product: Ores and Metals; Type: JSON...
r = Request Trade USA "OresMtls" JSON "ALL" "XPRT-TRD-VL" 2016

main :: IO ()
main = do
 d <- (eitherDecode <$> getData r) :: IO (Either String Object)
 case d of
  Left err -> putStrLn err
  Right ps -> print ps
