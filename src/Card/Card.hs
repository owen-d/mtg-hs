{-# LANGUAGE DuplicateRecordFields #-}


module Card.Card
  ( Color(..)
  , Colors
  , Cost(..)
  , CardType(..)
  , Rarity(..)
  , Card(..)
  ) where


data Color
  = White
  | Blue
  | Black
  | Red
  | Green
  deriving (Eq, Enum, Bounded, Show)

type Colors = [Color]

-- basically a linked list of different cost types
data Cost
  = Color
  | Colorless Int
  | Combined Cost
             Cost
  | Split Color
          Color
  | X
    deriving (Show)

data CardType
  = Artifact
  | Creature
  | Enchantment
  | Instant
  | Land
  | PlanesWalker
  deriving (Show)

data Rarity
  = Common
  | Uncommon
  | Rare
  | Mythic
  | Special
  | Basic
  deriving (Show)

data Card = Card
  { id :: String
  , name :: String
  , manaCost :: Cost
  , convertedManaCost :: Int
  , colorIdentity :: Colors
  , cardType :: CardType
  , rarity :: Rarity
  , text :: String
  , flavor :: String
  , artist :: String
  , number :: String
  , power :: Int
  , toughness :: Int
  , loyalty :: Int
  , multiverseId :: Int
  }
  deriving (Show)
