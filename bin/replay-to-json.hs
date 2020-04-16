import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.List
import Data.Monoid
import Dr.Mario.Model
import Parser (Ending(..), readGameRecord)
import System.Environment
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.Text as T

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
	-- outermost: rotation
	-- middlemost: x
	-- innermost: y
	, moves :: [[[Double]]]
	} deriving (Eq, Ord, Read, Show)

instance A.ToJSON TrainingExample where
	-- TODO: Since the border values in all channels are constant, and the
	-- final 4 channels are completely constant, should those values be the
	-- responsibility of the reader rather than the writer to save a bit of
	-- disk space and disk bandwidth?
	toJSON e = A.object
		[ T.pack "input" .=
			[
				[
					[ fromEnum $ get (board e) (Position x y) `elem` cells
					| y <- [-1..16]
					]
				| x <- [-1..8]
				]
			| shapes <- [[Virus], [Disconnected, North, South], [East], [West]]
			, color <- [Red, Yellow, Blue]
			, let cells = [Just (Occupied color shape) | shape <- shapes]
			] ++
			[ replicate 10 (replicate 18 (fromEnum (pillColor == channelColor)))
			| pillColor <- [fst (pill e), snd (pill e)]
			, channelColor <- [Red, Yellow, Blue]
			] ++
			[ replicate 18 1 : replicate 9 (replicate 18 0)
			, replicate 9 (replicate 18 0) ++ [replicate 18 1]
			, replicate 10 (1 : replicate 17 0)
			, replicate 10 (replicate 17 0 ++ [1])
			]
		, T.pack "output" .= A.object
			[ T.pack "won" .= 2*fromEnum (won e) - 1
			, T.pack "cleared" .= cleared e
			, T.pack "moves" .= moves e
			]
		] where
		-- Why does A..= bind so tightly? Who knows. Anyway, we want it to have a
		-- precedence of 4 or lower so ++ happens first.
		infixr 3 .=
		(.=) :: (A.KeyValue kv, A.ToJSON v) => T.Text -> v -> kv
		(.=) = (A..=)

examplesFromRecord :: (Board, [(Pill, HashMap Pill Double)], Ending) -> Maybe [TrainingExample]
examplesFromRecord (b_, ms_, e) = (\(_won, _cleared, es) -> es) <$> go b_ ms_ where
	go b [] = case e of
		End -> pure (getAll (ofoldMap isNotVirus b), 0, [])
		Stall -> pure (False, 0, [])
		Timeout -> Nothing
	go b ((p, ps):ms) = do
		(cleared, b') <- place b p
		(won, cleared', es) <- go b' ms
		let c = content p
		    cleared'' = cleared+cleared'
		    moves = movesFromVisitCounts c ps
		    e = TrainingExample b (bottomLeftColor c, otherColor c) won cleared'' moves
		pure (won, cleared'', e:es)

	isNotVirus (Occupied _ Virus) = All False
	isNotVirus _ = All True

movesFromVisitCounts :: PillContent -> HashMap Pill Double -> [[[Double]]]
movesFromVisitCounts c0_ ws =
	[
		[
			[ HM.lookupDefault 0 (Pill c (Position x y)) ws / w
			| y <- [0..15]
			]
		| x <- [0..7]
		]
	| c <- take 4 $ iterate (`rotateContent` Clockwise) c0
	]
	where
	w = sum ws * if bottomLeftColor c0 == otherColor c0 then 2 else 1
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

	mirrorMoves [zero, one, two, three] = [horizReverse two, reverse one, horizReverse zero, reverse three]
	horizReverse xs = reverse (init xs) ++ [last xs]

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
		| (rotation, scoress) <- zip [0..] (moves e)
		, (x, scores) <- zip [0..] scoress
		, (y, score) <- zip [0..] scores
		, score /= 0
		]
