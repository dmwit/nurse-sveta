module Util where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Applicative
import Data.List
import Data.Maybe
import Dr.Mario.Sveta
import Dr.Mario.Sveta.MCTS
import Graphics.Vty
import System.Random.MWC
import qualified Dr.Mario.Model as M

randomBoard :: GenIO -> IO M.Board
randomBoard gen = liftA2 M.randomBoard
	(uniformR (2, maxBound) gen) (uniformR (0, 20) gen)

randomPill :: GenIO -> IO (M.Color, M.Color)
randomPill gen = liftA2 (,)
	(M.decodeColor <$> uniformR (2, maxBound) gen)
	(M.decodeColor <$> uniformR (2, maxBound) gen)

data Repetitions = Finite Int | Infinity deriving (Eq, Ord, Read, Show)

toggleFiniteness :: Repetitions -> Repetitions
toggleFiniteness (Finite _) = Infinity
toggleFiniteness Infinity = Finite 0

increment :: Repetitions -> Repetitions
increment (Finite n) = Finite (n+1)
increment Infinity = Infinity

decrement :: Repetitions -> Repetitions
decrement (Finite n) = Finite (n-1)
decrement Infinity = Infinity

data Comms = Comms
	{ tree :: DrMarioTree
	, params :: DrMarioParameters
	, repetitions :: Repetitions
	, sequenceNumber :: Int
	, iterations :: Double
	}

mctsThread :: TVar Comms -> IO ()
mctsThread commsRef = forever $ do
	c <- atomically $ do
		c <- readTVar commsRef
		guard (repetitions c > Finite 0)
		pure c
	t <- mcts (params c) (tree c)
	atomically $ do
		c' <- readTVar commsRef
		if sequenceNumber c == sequenceNumber c'
		then writeTVar commsRef c
			{ tree = t
			, repetitions = decrement (repetitions c)
			, iterations = iterations c + 1
			}
		else pure ()

renderLookaheadFor :: M.Board -> M.Color -> M.Color -> Image
renderLookaheadFor b l r = horizCat
	[ string defAttr padding
	, renderCell (M.Occupied l M.West)
	, char defAttr ' '
	, renderCell (M.Occupied r M.East)
	] where padding = replicate (2*(M.width b `div` 2 - 1)) ' '

renderBoard :: M.Board -> (M.Position -> Maybe M.Cell) -> Image
renderBoard b overlay = vertCat
	[ horizCat . intersperse (char defAttr ' ') $
		[ renderCell (fromMaybe (M.unsafeGet b pos) (overlay pos))
		| x <- [0 .. M.width b-1]
		, let pos = M.Position x y
		]
	| y <- [M.height b-1, M.height b-2 .. 0]
	]

pillOverlay :: M.Pill -> M.Position -> Maybe M.Cell
pillOverlay p pos
	| pos == M.bottomLeftPosition p = Just (M.bottomLeftCell (M.content p))
	| pos == M.otherPosition p = Just (M.otherCell (M.content p))
	| otherwise = Nothing

renderBoardWithPill :: M.Board -> Maybe M.Pill -> Image
renderBoardWithPill b mp = renderBoard b $ case mp of
	Nothing -> const Nothing
	Just p -> pillOverlay p

renderCell :: M.Cell -> Image
renderCell M.Empty = char defAttr ' '
renderCell (M.Occupied c s) = char (colorAttr c) (shapeChar s)

colorAttr :: M.Color -> Attr
colorAttr c = defAttr `withForeColor` case c of
	M.Red    -> red
	M.Blue   -> cyan
	M.Yellow -> yellow

shapeChar :: M.Shape -> Char
shapeChar M.Virus        = '☻'
shapeChar M.Disconnected = '○'
shapeChar M.North        = '∩'
shapeChar M.South        = '∪'
shapeChar M.East         = '⊃'
shapeChar M.West         = '⊂'
