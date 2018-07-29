module Card
    (
    ) where


data Color
  = White
  | Blue
  | Black
  | Red
  | Green
  deriving (Eq, Enum, Bounded, Show)

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

