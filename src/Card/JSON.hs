{-# LANGUAGE OverloadedStrings #-}

module Card.JSON
  (
  ) where

import           Card.Card
import           Data.Aeson.Types

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V

parseColor :: Value -> Parser Color
parseColor (String "White") = return White
parseColor (String "Blue")  = return Blue
parseColor (String "Black") = return Black
parseColor (String "Red")   = return Red
parseColor (String "Green") = return Green
parseColor _                = fail "expected a Color"

instance FromJSON Color where
  parseJSON = parseColor

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

parseCost :: T.Text -> Parser Cost
parseCost s =
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

instance FromJSON Cost where
  parseJSON = withText "Cost" parseCost

tokens :: String -> String
tokens s =
  let extract = head . tail . (take 3)
      rest = drop 3
      ok = (>= 2) . length
  in if ok s
       then (extract s) : (tokens $ rest s)
       else []

parseTokens :: Object -> Parser Costs
parseTokens o = case HM.lookup "manaCost" o of
  Just (String v) -> parseJSON . String . T.pack . tokens . T.unpack $ v
  Just _          -> fail "invalid manaCost type"
  Nothing         -> fail "key manaCost not present"

instance FromJSON Card where
  parseJSON = withObject "Card" $ \v -> Card
    <$> v .: "id"
    <*> v .: "name"
    <*> (parseTokens v)
    <*> v .: "convertedManaCost"
    <*> v .: "colorIdentity"
    <*> v .: "cardTypes"
    <*> v .: "rarity"
    <*> v .: "text"
    <*> v .: "flavor"
    <*> v .: "artist"
    <*> v .: "number"
    <*> v .: "power"
    <*> v .: "toughtness"
    <*> v .: "loyalty"
    <*> v .: "multiverseId"
