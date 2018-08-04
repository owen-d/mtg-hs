module Card.Utils
  (
  ) where

import           Card.Card
import           Card.JSON

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as B

jsonFile :: FilePath
jsonFile = "/tmp/cards.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

a :: IO ()
a = ((A.eitherDecode <$> getJSON) :: IO (Either String [Card])) >>= (log' 2)
-- a = do
--   cards <- (A.eitherDecode <$> getJSON) :: IO (Either String [Card])
--   case cards of
--     Left err -> putStrLn err
--     Right cards' ->
--       let wanted = take 5 cards'
--       in mapM_ print wanted

log' :: Show a => Int ->  Either String [a] -> IO ()
log' n a =
  case a of
    Left err -> putStrLn err
    Right a' -> mapM_ print $ take n a'
