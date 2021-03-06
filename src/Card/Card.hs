{-# LANGUAGE DuplicateRecordFields #-}


module Card.Card
  ( Color(..)
  , Colors
  , Cost(..)
  , Costs
  , CardType(..)
  , CardTypes
  , Rarity(..)
  , Card(..)
  , CombatStat(..)
  ) where


data Color
  = White
  | Blue
  | Black
  | Red
  | Green
  deriving (Eq, Enum, Bounded, Show)

type Colors = [Color]

data Cost
  = One Color
  | Colorless Int
  | Split Color
          Color
  | X
    deriving (Show)

type Costs = [Cost]

data CardType
  = Artifact
  | Creature
  | Enchantment
  | Instant
  | Sorcery
  | Land
  | PlanesWalker
  deriving (Show)

type CardTypes = [CardType]

data Rarity
  = Common
  | Uncommon
  | Rare
  | Mythic
  | Special
  | Basic
  deriving (Show)

data CombatStat
  = Stat Int
  | Wildcard
  deriving (Show)

data Card = Card
  { id            :: String
  , name          :: String
  , manaCost      :: Costs
  -- , convertedManaCost :: Int
  , colorIdentity :: Colors
  , cardTypes     :: CardTypes
  , rarity        :: Rarity
  , text          :: Maybe String
  , flavor        :: Maybe String
  , artist        :: String
  , number        :: String
  , power         :: Maybe CombatStat
  , toughness     :: Maybe CombatStat
  , loyalty       :: Maybe Int
  , multiverseId  :: Int
  }
  deriving (Show)
