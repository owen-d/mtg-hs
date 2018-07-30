{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Card.JSON
  (
  ) where

import           Card.Card
import           Data.Aeson.Types

import qualified Data.Text        as T
import qualified Data.Vector      as V

parseColor :: Value -> Parser Color
parseColor (String "White") = return White
parseColor (String "Blue")  = return Blue
parseColor (String "Black") = return Black
parseColor (String "Red")   = return Red
parseColor (String "Green") = return Green
parseColor _                = fail "expected a Color"

instance FromJSON Color where
  parseJSON = parseColor

-- instance FromJSON [Color] where
--   parseJSON =
--     withArray "array of Colors" $ \arr -> mapM parseColor (V.toList arr)

parseCardType :: Value -> Parser CardType
parseCardType (String "Artifact")     = return Artifact
parseCardType (String "Creature")     = return Creature
parseCardType (String "Enchantment")  = return Enchantment
parseCardType (String "Instant")      = return Instant
parseCardType (String "Land")         = return Land
parseCardType (String "PlanesWalker") = return PlanesWalker
parseCardType _                       = fail "expected a CardType"

instance FromJSON CardType where
  parseJSON = parseCardType

parseRarity :: Value -> Parser Rarity
parseRarity (String "Common")   = return Common
parseRarity (String "Uncommon") = return Uncommon
parseRarity (String "Rare")     = return Rare
parseRarity (String "Mythic")   = return Mythic
parseRarity (String "Special")  = return Special
parseRarity (String "Basic")    = return Basic
parseRarity _                   = fail "expected a Rarity"

instance FromJSON Rarity where
  parseJSON = parseRarity

parseCost :: Value -> Parser Cost
parseCost (String s) =
  case s of
    "W" -> return $ One White
    "B" -> return $ One Black
    "U" -> return $ One Blue
    "R" -> return $ One Red
    "G" -> return $ One Green
    "X" -> return X
   -- Issue: only handles 1-9
    n | elem n' ['1'..'9'] ->
        return $ Colorless $ read [n']
      where n' = T.head n
    _ -> fail "Expected a Cost"
parseCost _ = fail "Expected a Cost"

instance FromJSON Cost where
  parseJSON = parseCost

tokens :: String -> String
tokens s =
  let extract = head . tail . (take 3)
      rest = drop 3
      ok = (>= 2) . length
  in
    if ok s
    then (extract s):(tokens $ rest s)
    else []

parseCard :: Value -> Parser Card
parseCard (Object o) = do
  id <- o .: "id"
  name <- o .: "name"
  text <- o .: "text"
  flavor <- o .: "flavor"
  artist <- o .: "artist"
  number <- o .: "number"
  power <- o .: "power"
  toughness <- o .: "toughness"
  loyalty <- o .: "id"
  mulitverseId <- o .: "mulitverseId"
  -- customs
  manaCost <- o .: "manaCost"
  convertedManaCost <- o .: "convertedManaCost"
  colorIdentity <- o .: "colorIdentity"
  cardType <- o .: "cardType"
  rarity <- o .: "rarity"
