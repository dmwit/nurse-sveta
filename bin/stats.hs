{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Csv
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Monoid
import Data.Vector (Vector)
import Dr.Mario.Model
import Options.Applicative
import Parser
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Options.Applicative.Help.Pretty as D
import qualified Options.Applicative.Help.Chunk as D

main :: IO ()
main = do
	opts <- execParser options
	stats <- gather (rootDirectory opts)
	case mode opts of
		Summary -> LC8.putStr $ encodeDefaultOrderedByName stats
		Tournament -> LC8.putStr . encodeDefaultOrderedByName =<< runTournaments stats

data Mode = Tournament | Summary deriving (Bounded, Enum, Eq, Ord, Read, Show)
data StatOptions = StatOptions
	{ rootDirectory :: FilePath
	, mode :: Mode
	} deriving (Eq, Ord, Read, Show)

options :: ParserInfo StatOptions
options = info (helper <*> parser)
	(  fullDesc
	<> progDesc "Gather some statistics about game records."
	) where
	parser = pure StatOptions
		<*> argument str
			(  metavar "DIR"
			<> help "The root directory to (recursively) scan for game records."
			)
		<*> option mode
			(  long "mode"
			<> short 'm'
			<> helpChunk
				(      D.paragraph "tournament: pairwise WLT statistics"
				<<$$>> D.paragraph "summary: evaluation metrics suitable for use in another analysis tool"
				)
			<> value Summary
			<> metavar "MODE"
			)
	mode = maybeReader $ \case
		"tournament" -> Just Tournament
		"summary" -> Just Summary
		_ -> Nothing
	helpChunk = helpDoc . D.unChunk
	(<<$$>>) = D.chunked (D.<$$>)

usage :: String -> String
usage progName = ""
	++ "USAGE: " ++ progName ++ " DIRECTORY\n"
	++ "Gather some statistics about game records stored in the given directory. They\n"
	++ "will be printed in CSV format to stdout."

data GameInformation = GameInformation
	{ directory :: FilePath
	, file :: FilePath
	, startingViruses :: Int
	, endingViruses :: Int
	, pillsUsed :: Int
	, ending :: Ending
	, finalJunk :: ColorMap Int
	} deriving (Eq, Ord, Read, Show)

gather :: FilePath -> IO [GameInformation]
gather = go "" where
	go dir file = goDirectory <|> goFile <|> complain where
		goDirectory = do
			let dir' = dir </> file
			ps <- listDirectory dir'
			concat <$> traverse (go dir') ps
		goFile = readGameRecord (dir </> file) >>= summarize dir file
		complain = [] <$ hPutStrLn stderr ("Skipping unparseable game record in " ++ (dir </> file))

summarize :: FilePath -> FilePath -> (Board, [(Pill, a)], Ending) -> IO [GameInformation]
summarize dir file (b, ps, e) = case boardStatesFromPills b (map fst ps) >>= finalBoard of
	Left (b,p) -> fail ("Cannot place pill " ++ show p ++ " on board " ++ show b)
	Right b' -> pure . pure $ GameInformation
		{ directory = dir
		, file = file
		, startingViruses = countViruses b
		, endingViruses = countViruses b'
		, pillsUsed = length ps
		, ending = e
		, finalJunk = countJunk b'
		}

finalBoard :: [BoardState] -> Either (Board, Pill) Board
finalBoard bs = case last bs of
	BoardState { board = b, placement = Just p } -> case place b p of
		Just (_, b') -> Right b'
		Nothing -> Left (b, p)
	s -> Right (board s)

countViruses :: Board -> Int
countViruses = getSum . ofoldMap (countMaybe Virus . shape)

countJunk :: Board -> ColorMap Int
countJunk = fmap getSum . ofoldMap go where
	go = liftA3 colorMap (countMaybe Red) (countMaybe Yellow) (countMaybe Blue) . color

countMaybe :: Eq a => a -> Maybe a -> Sum Int
countMaybe needle (Just haystack) | needle == haystack = 1
countMaybe _ _ = 0

groupByKey :: (Hashable k, Eq k) => (v -> k) -> [v] -> HashMap k [v]
groupByKey f vs = HM.fromListWith (++) [(f v, [v]) | v <- vs]

groupByFile :: [GameInformation] -> IO (HashMap FilePath (HashMap FilePath GameInformation))
groupByFile = id
	. HM.traverseWithKey (\dir -> HM.traverseWithKey (\file -> the dir file))
	. fmap (groupByKey file)
	. groupByKey directory
	where
	the _ _ [v] = pure v
	the dir file _ = die $
		"The impossible happened! Multiple game information records for " ++ (dir </> file) ++ "."

data TournamentResult = TournamentResult
	{ wins, losses, ties, winsByDefault, lossesByDefault :: Int }
	deriving (Eq, Ord, Read, Show)

instance Semigroup TournamentResult where
	r1 <> r2 = TournamentResult
		{ wins = wins r1 + wins r2
		, losses = losses r1 + losses r2
		, ties = ties r1 + ties r2
		, winsByDefault = winsByDefault r1 + winsByDefault r2
		, lossesByDefault = lossesByDefault r1 + lossesByDefault r2
		}

instance Monoid TournamentResult where
	mempty = TournamentResult
		{ wins = 0
		, losses = 0
		, ties = 0
		, winsByDefault = 0
		, lossesByDefault = 0
		}

singleGameMatchup :: GameInformation -> GameInformation -> TournamentResult
singleGameMatchup gi1 gi2
	| startingViruses gi1 /= startingViruses gi2 = error "The impossible happened! Two ostensibly identical games started with different virus counts."
	| otherwise = case compare (endingViruses gi1, pillsUsed gi1, sum (finalJunk gi1))
	                           (endingViruses gi2, pillsUsed gi2, sum (finalJunk gi2)) of
		LT -> mempty { wins = 1 }
		EQ -> mempty { ties = 1 }
		GT -> mempty { losses = 1 }

singleTournament :: HashMap FilePath GameInformation -> HashMap FilePath GameInformation -> TournamentResult
singleTournament gis1 gis2 = mempty
	<> fold (HM.intersectionWith singleGameMatchup gis1 gis2)
	<> mempty
		{ winsByDefault = HM.size (HM.difference gis1 gis2)
		, lossesByDefault = HM.size (HM.difference gis2 gis1)
		}

allTournaments :: HashMap FilePath (HashMap FilePath GameInformation) -> HashMap (FilePath, FilePath) TournamentResult
allTournaments giss = HM.fromList
	[ ((dir1, dir2), singleTournament gis1 gis2)
	| (dir1, gis1):giss' <- tails (HM.toList giss)
	, (dir2, gis2) <- giss'
	]

data TournamentInformation = TournamentInformation
	{ team1, team2 :: FilePath
	, result :: TournamentResult
	} deriving (Eq, Ord, Read, Show)

runTournaments :: [GameInformation] -> IO [TournamentInformation]
runTournaments = id
	. fmap (id
		. map (\((t1, t2), r) -> TournamentInformation t1 t2 r)
		. HM.toList
		. allTournaments
		)
	. groupByFile

instance ToNamedRecord GameInformation where
	toNamedRecord gi = HM.fromList . tail $ [undefined
		, "directory" .-> UTF8.fromString (directory gi)
		, "file" .-> UTF8.fromString (file gi)
		, "starting-virus-count" .-> bshow (startingViruses gi)
		, "cleared-virus-count" .-> bshow (startingViruses gi - endingViruses gi)
		, "uncleared-virus-count" .-> bshow (endingViruses gi)
		, "pill-count" .-> bshow (pillsUsed gi)
		, "ending" .-> bshow (ending gi)
		, "uncleared-junk" .-> bshow (sum (finalJunk gi))
		, "uncleared-red-junk" .-> bshow (junk Red)
		, "uncleared-yellow-junk" .-> bshow (junk Yellow)
		, "uncleared-blue-junk" .-> bshow (junk Blue)
		] where ColorMap junk = finalJunk gi

instance DefaultOrdered GameInformation where
	headerOrder _ = V.fromList . map C8.pack . tail $ [undefined
		, "directory"
		, "file"
		, "starting-virus-count"
		, "cleared-virus-count"
		, "uncleared-virus-count"
		, "pill-count"
		, "ending"
		, "uncleared-junk"
		, "uncleared-red-junk"
		, "uncleared-yellow-junk"
		, "uncleared-blue-junk"
		]

instance ToNamedRecord TournamentInformation where
	toNamedRecord ti = HM.fromList . tail $ [undefined
		, "home-team" .-> UTF8.fromString (team1 ti)
		, "away-team" .-> UTF8.fromString (team2 ti)
		, "outright-wins" .-> bshow (wins r)
		, "outright-losses" .-> bshow (losses r)
		, "ties" .-> bshow (ties r)
		, "default-wins" .-> bshow (winsByDefault r)
		, "default-losses" .-> bshow (lossesByDefault r)
		] where r = result ti

instance DefaultOrdered TournamentInformation where
	headerOrder _ = V.fromList . map C8.pack . tail $ [undefined
		, "home-team"
		, "away-team"
		, "outright-wins"
		, "outright-losses"
		, "ties"
		, "default-wins"
		, "default-losses"
		]

(.->) :: String -> ByteString -> (ByteString, ByteString)
s .-> bs = (C8.pack s, bs)

bshow :: Show a => a -> ByteString
bshow = C8.pack . show

newtype ColorMap a = ColorMap (Color -> a) deriving (Functor, Applicative, Monad, Semigroup, Monoid)

colors :: [Color]
colors = [Red, Yellow, Blue]

colorMap :: a -> a -> a -> ColorMap a
colorMap r y b = ColorMap $ \c -> case c of
	Red -> r
	Yellow -> y
	Blue -> b

instance Eq a => Eq (ColorMap a) where ColorMap f == ColorMap g = map f colors == map g colors
instance Ord a => Ord (ColorMap a) where compare (ColorMap f) (ColorMap g) = compare (map f colors) (map g colors)
instance Show a => Show (ColorMap a) where show (ColorMap f) = show [(c, f c) | c <- colors]
instance Read a => Read (ColorMap a) where
	readsPrec n s = readsPrec n s >>= fromList where
		fromList ([(Red, r), (Yellow, y), (Blue, b)], s) = pure . flip (,) s $ colorMap r y b
		fromList _ = []
instance Traversable ColorMap where
	traverse f (ColorMap g) = pure colorMap
		<*> f (g Red)
		<*> f (g Yellow)
		<*> f (g Blue)
instance Foldable ColorMap where foldMap f (ColorMap g) = f (g Red) <> f (g Yellow) <> f (g Blue)
