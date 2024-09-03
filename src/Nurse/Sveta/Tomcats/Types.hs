module Nurse.Sveta.Tomcats.Types (
	module Nurse.Sveta.Tomcats.Types,
	module Tomcats.AlphaZero.Float,
	) where

import Control.Applicative
import Data.Aeson
import Data.Bits
import Data.Hashable
import Dr.Mario.Model
import Dr.Mario.Pathfinding

import Tomcats.AlphaZero.Float (Statistics(..))

-- | The 'MidPath' is ignored for 'Eq', 'Ord', and 'Hashable'.
data Move = RNG Color Color | Placement MidPath Pill deriving (Show, Read)

instance Eq Move where
	RNG l0 r0 == RNG l1 r1 = l0 == l1 && r0 == r1
	Placement _ l == Placement _ r = l == r
	_ == _ = False

instance Ord Move where
	compare (RNG l0 l1) (RNG r0 r1) = compare l0 r0 <> compare l1 r1
	compare (Placement _ l) (Placement _ r) = compare l r
	compare RNG{} _ = LT
	compare Placement{} _ = GT

instance Hashable Move where
	hashWithSalt s = \case
		RNG c c'         ->            s `hashWithSalt` c `hashWithSalt` c'
		Placement _ pill -> complement s `hashWithSalt` pill

instance ToJSON Move where
	toJSON = \case
		RNG l r -> toJSON [l, r]
		Placement m p -> toJSON (m, p)
	toEncoding = \case
		RNG l r -> toEncoding [l, r]
		Placement m p -> toEncoding (m, p)

instance FromJSON Move where
	parseJSON v = parseRNG <|> parsePlacement where
		parseRNG = do
			[l, r] <- parseJSON v
			pure (RNG l r)
		parsePlacement = uncurry Placement <$> parseJSON v
