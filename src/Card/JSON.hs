{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Card.JSON
  (
  ) where

import Data.Aeson.Types
import Card.Card

import qualified Data.Vector as V

parseColor :: Value -> Parser Color
parseColor (String "White") = return White
parseColor (String "Blue") = return Blue
parseColor (String "Black") = return Black
parseColor (String "Red") = return Red
parseColor (String "Green") = return Green
parseColor _ = fail "expected a Color"

instance FromJSON Color where
  parseJSON = parseColor

-- instance FromJSON [Color] where
--   parseJSON =
--     withArray "array of Colors" $ \arr -> mapM parseColor (V.toList arr)


parseCardType :: Value -> Parser CardType
parseCardType (String "Artifact") = return Artifact
parseCardType (String "Creature") = return Creature
parseCardType (String "Enchantment") = return Enchantment
parseCardType (String "Instant") = return Instant
parseCardType (String "Land") = return Land
parseCardType (String "PlanesWalker") = return PlanesWalker
parseCardType _ = fail "expected a CardType"

instance FromJSON CardType where
  parseJSON = parseCardType

parseRarity :: Value -> Parser Rarity
parseRarity (String "Common") = return Common
parseRarity (String "Uncommon") = return Uncommon
parseRarity (String "Rare") = return Rare
parseRarity (String "Mythic") = return Mythic
parseRarity (String "Special") = return Special
parseRarity (String "Basic") = return Basic
parseRarity _ = fail "expected a Rarity"

instance FromJSON Rarity where
  parseJSON = parseRarity
