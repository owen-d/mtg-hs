{-# LANGUAGE OverloadedStrings #-}

module Card.JSON
  ( module Card.JSON
  ) where

import           Card.Card
import           Data.Aeson.Types

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

parseColor :: Value -> Parser Color
parseColor (String "W") = return White
parseColor (String "U") = return Blue
parseColor (String "B") = return Black
parseColor (String "R") = return Red
parseColor (String "G") = return Green
parseColor _            = fail "expected a Color"

instance FromJSON Color where
  parseJSON = parseColor

parseCardType :: Value -> Parser CardType
parseCardType (String "Artifact")     = return Artifact
parseCardType (String "Creature")     = return Creature
parseCardType (String "Enchantment")  = return Enchantment
parseCardType (String "Instant")      = return Instant
parseCardType (String "Sorcery")      = return Sorcery
parseCardType (String "Land")         = return Land
parseCardType (String "Planeswalker") = return PlanesWalker
parseCardType _                       = fail "expected a CardType"

instance FromJSON CardType where
  parseJSON = parseCardType

parseRarity :: Value -> Parser Rarity
parseRarity (String "Common")      = return Common
parseRarity (String "Uncommon")    = return Uncommon
parseRarity (String "Rare")        = return Rare
parseRarity (String "Mythic Rare") = return Mythic
parseRarity (String "Special")     = return Special
parseRarity (String "Basic")       = return Basic
parseRarity _                      = fail "expected a Rarity"

instance FromJSON Rarity where
  parseJSON = parseRarity

-- Since string is itself a list, we wrap it in tagged union.
-- Otherwise it wont register as [a] in parsing, but rather a
data CostWrapper = CostWrapper Char
  deriving (Show)

parseCost :: CostWrapper -> Parser Cost
parseCost (CostWrapper s) =
  case s of
    'W' -> return $ One White
    'B' -> return $ One Black
    'U' -> return $ One Blue
    'R' -> return $ One Red
    'G' -> return $ One Green
    'X' -> return X
   -- Issue: only handles 1-9
    n | elem n ['1'..'9'] ->
        return $ Colorless $ read [n]
    _ -> fail "Expected a Cost"

tokens :: String -> [CostWrapper]
tokens s =
  let extract = head . tail . (take 3)
      rest = drop 3
      ok = (>= 2) . length
  in if ok s
       then (CostWrapper . extract $ s) : (tokens $ rest s)
       else []

parseTokens :: Object -> Parser Costs
parseTokens o = case HM.lookup "manaCost" o of
  Just (String v) ->
    sequence . map parseCost $ tokens . T.unpack $ v
  Just _          -> fail "invalid manaCost type"
  Nothing         -> fail "key manaCost not present"

parseIntToString :: Value -> Parser Int
parseIntToString (String s) =
  let
    converted = reads (T.unpack s) :: [(Int, String)]
  in case converted of
    (n,_):_ -> return n
    _       -> fail "failure to parse"
parseIntToString _ = fail "Invalid type for parsing"

instance FromJSON Card where
  parseJSON = withObject "Card" $ \v -> Card
    <$> v .: "id"
    <*> v .: "name"
    <*> (parseTokens v)
    -- <*> v .: "convertedManaCost"
    <*> v .: "colorIdentity"
    <*> v .: "types"
    <*> v .: "rarity"
    <*> v .:? "text"
    <*> v .:? "flavor"
    <*> v .: "artist"
    <*> v .: "number"
    <*> parsePower v
    <*> parseToughness v
    <*> v .:? "loyalty"
    <*> v .: "multiverseid"
    where
      parsePower o =
        explicitParseFieldMaybe parseIntToString o "power"
      parseToughness o =
        explicitParseFieldMaybe parseIntToString o "toughness"
