import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.List
import Data.Monoid
import Dr.Mario.Model
import Parser (Ending(..), readGameRecord)
import System.Environment
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Dr.Mario.Protocol.Raw as Proto

main :: IO ()
main = do
	[i, o] <- getArgs
	Just examples <- examplesFromRecord <$> readGameRecord i
	A.encodeFile o (generateVariants examples)

data TrainingExample = TrainingExample
	{ board :: Board
	, pill :: (Color, Color)
	, won :: Bool
	, cleared :: Int
	, duration :: Int
	-- one element per rotation
	, moves :: [HashMap Position Double]
	} deriving (Eq, Ord, Read, Show)

instance A.ToJSON TrainingExample where
	toJSON e = A.toJSON
		[ protoJSON (board e)
		, protoJSON (PillContent Horizontal l r)
		, A.toJSON (if won e then 1 else -1 :: Int)
		, A.toJSON (cleared e)
		, A.toJSON (duration e)
		, A.toJSON . map sparseJSON $ moves e
		] where
		(l, r) = pill e

		-- oof
		protoJSON v = A.toJSON . T.decodeLatin1 . BS.toStrict . BS.toLazyByteString . snd . Proto.pp $ v

		sparseJSON ws = A.toJSON
			[ v
			| (Position x y, w) <- HM.toList ws
			, v <- [fromIntegral x, fromIntegral y, w]
			]

examplesFromRecord :: (Board, [(Pill, HashMap Pill Double)], Ending) -> Maybe [TrainingExample]
examplesFromRecord (b_, ms_, e) = (\(_won, _cleared, _duration, es) -> es) <$> go b_ ms_ where
	go b [] = case e of
		End -> pure (getAll (ofoldMap isNotVirus b), 0, 0, [])
		Stall -> pure (False, 0, 0, [])
		Timeout -> Nothing
	go b ((p, ps):ms) = do
		(cleared, b') <- place b p
		(won, cleared', duration, es) <- go b' ms
		let c = content p
		    cleared'' = cleared+cleared'
		    moves = movesFromVisitCounts c ps
		    duration' = duration + 1
		    e = TrainingExample b (bottomLeftColor c, otherColor c) won cleared'' duration' moves
		pure (won, cleared'', duration', e:es)

	isNotVirus (Occupied _ Virus) = All False
	isNotVirus _ = All True

movesFromVisitCounts :: PillContent -> HashMap Pill Double -> [HashMap Position Double]
movesFromVisitCounts c0_ ws =
	[ HM.fromList [(pos, w / sumw) | (Pill c' pos, w) <- HM.toList ws, c' == c]
	| c <- take 4 $ iterate (`rotateContent` Clockwise) c0
	]
	where
	sumw = sum ws * if bottomLeftColor c0 == otherColor c0 then 2 else 1
	c0 = c0_ { orientation = Horizontal }

-- This is unsafe because it assumes the incoming list is a permutation of
-- [Red, Yellow, Blue].
unsafePermuteColors :: [Color] -> TrainingExample -> TrainingExample
unsafePermuteColors [newRed, newYellow, newBlue] e = e
	{ board = unsafeMap permuteCell (board e)
	, pill = permutePill (pill e)
	} where
	permuteCell (Occupied color shape) = Occupied (permuteColor color) shape
	permuteCell Empty = Empty

	permutePill (l, r) = (permuteColor l, permuteColor r)

	permuteColor Red = newRed
	permuteColor Yellow = newYellow
	permuteColor Blue = newBlue

-- These next two (mirrorBoard and mirrorPill) are sort of only mostly correct:
-- there are situations where mirroring the board or mirroring the pill changes
-- what pill placements are possible to reach in time. However, for that to
-- happen, the forced-drop speed must be four frames or fewer, which is very
-- unusual, so let's just assume in this draft that it never happens.

mirrorBoard :: TrainingExample -> TrainingExample
mirrorBoard e = e
	{ board = go (board e)
	, moves = mirrorMoves (moves e)
	} where
	go b = unsafeGenerateBoard (width b) (height b) (\(Position x y) -> mirrorCell . unsafeGet b $ Position (width b - x - 1) y)

	mirrorCell (Occupied c East) = Occupied c West
	mirrorCell (Occupied c West) = Occupied c East
	mirrorCell c = c

	mirrorMoves [zero, one, two, three] = [horizMirrorPosition two, vertMirrorPosition one, horizMirrorPosition zero, vertMirrorPosition three]
	horizMirrorPosition ws = HM.fromList [(Position (6-x) y, w) | (Position x y, w) <- HM.toList ws]
	vertMirrorPosition  ws = HM.fromList [(Position (7-x) y, w) | (Position x y, w) <- HM.toList ws]

mirrorPill :: TrainingExample -> TrainingExample
mirrorPill e = e
	{ pill = go (pill e)
	, moves = mirrorMoves (moves e)
	} where
	go (l, r) = (r, l)
	mirrorMoves [zero, one, two, three] = [two, three, zero, one]

generateVariants :: [TrainingExample] -> [TrainingExample]
generateVariants es = do
	perm <- permutations [Red, Yellow, Blue]
	e0 <- unsafePermuteColors perm <$> es
	e1 <- e0 : [mirrorPill e0 | uncurry (/=) (pill e0)]
	[e1, mirrorBoard e1]

ppTrainingExample :: TrainingExample -> String
ppTrainingExample e = unlines
	[ ['<', ppColor l, '>', ppColor r, ' ', ppWon (won e), ' '] ++ show (cleared e)
	, pp (board e)
	, intercalate ", " (map ppPlacement placements)
	] where
	(l, r) = pill e
	ppColor = toLower . head . show

	ppWon False = 'x'
	ppWon True = '🗸'

	ppPlacement (rotation, x, y, _) = unwords
		[ [ if even rotation then '<' else 'v'
		  , ppColor $ if rotation == 0 || rotation == 3 then l else r
		  , if even rotation then '>' else '^'
		  , ppColor $ if rotation == 0 || rotation == 3 then r else l
		  ]
		, show x
		, show y
		]

	placements = sortOn (\(_, _, _, score) -> -score)
		[ (rotation, x, y, score)
		| (rotation, scores) <- zip [0..] (moves e)
		, (Position x y, score) <- HM.toList scores
		, score /= 0 -- probably not needed, but what the heck
		]
