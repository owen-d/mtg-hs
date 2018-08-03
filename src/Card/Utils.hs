module Card.Utils
  (
  ) where

import           Card.Card
import           Card.JSON

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as B

jsonFile :: FilePath
jsonFile = "/tmp/card.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

stuff :: IO ()
stuff = do
  d <- (A.eitherDecode <$> getJSON) :: IO (Either String Card)
  case d of
    Left err -> putStrLn err
    Right ps -> print ps
