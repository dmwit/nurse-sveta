module Main where

import CategoryMetadata
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Foldable
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Nurse.Sveta.Files
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
	args <- parseArgs <$> getArgs
	when (aHelp args) (usage ExitSuccess stdout)
	unless (null (aUnrecognized args)) do
		putStrLn . unwords $ "Unrecognized arguments:":aUnrecognized args
		usage (ExitFailure 1) stderr
	runApp args top

top :: App ()
top = do
	potentialCategories <- listDirectoryM (GamesProcessed "")
	categories <- fold <$> traverse readMetadata potentialCategories
	plan <- M.traverseWithKey planCategory categories
	proc <- planProcessor
	() <$ M.traverseWithKey proc plan

data CategoryConstraints = CategoryConstraints
	{ minFilename :: FilePath
	, otherFilenames :: Set FilePath
	} deriving (Eq, Ord, Read, Show)

data CategoryPlan = CategoryPlan
	{ move :: Set FilePath
	, live :: Set FilePath
	, magic :: Set FilePath
	, other :: Set FilePath
	} deriving (Eq, Ord, Read, Show)

instance Semigroup CategoryPlan where
	p <> p' = CategoryPlan
		{ move = move p <> move p'
		, live = live p <> live p'
		, magic = magic p <> magic p'
		, other = other p <> other p'
		}

instance Monoid CategoryPlan where
	mempty = CategoryPlan
		{ move = S.empty
		, live = S.empty
		, magic = S.empty
		, other = S.empty
		}

planProcessor :: App (String -> CategoryPlan -> App ())
planProcessor = do
	dry <- asks eDryRun
	pure $ if dry then describePlan else executePlan

executePlan :: String -> CategoryPlan -> App ()
executePlan category plan = do
	unless (S.null (move plan)) do
		dir <- directoryNameM (GamesArchive category)
		explain $ "Making sure " <> dir <> " exists"
		liftIO $ createDirectoryIfMissing True dir
	for_ (move plan) \fp -> do
		src <- fileNameM (GamesProcessed category) fp
		dst <- fileNameM (GamesArchive category) fp
		explain $ "Moving " <> fp
		liftIO $ renameFile src dst

describePlan :: String -> CategoryPlan -> App ()
describePlan category plan = do
	src <- directoryNameM (GamesProcessed category)
	dst <- directoryNameM (GamesArchive category)
	printFiles (printf "These files will be moved from %s to %s:\n" src dst) move
	whenVerbose do
		let notMoved :: String -> String
		    notMoved = printf "These files will not be moved out of %s because they %s:" src
		printFiles (notMoved "are metadata") magic
		printFiles (notMoved "may still be live") live
		printFiles (notMoved "are mentioned in the metadata") other
	where
	printFiles header f = when (not (S.null (f plan))) $ liftIO do
		putStrLn header
		for_ (f plan) (putStrLn . ('\t':))

planCategory :: String -> CategoryConstraints -> App CategoryPlan
planCategory category constraints = foldMap planFile <$> listDirectoryM (GamesProcessed category) where
	planFile fp
		| fp == metadataFilename = mempty { magic = S.singleton fp }
		| fp == namesFilename = mempty { magic = S.singleton fp }
		| fp >= minFilename constraints = mempty { live = S.singleton fp }
		| fp `S.member` otherFilenames constraints = mempty { other = S.singleton fp }
		| otherwise = mempty { move = S.singleton fp }

readMetadata :: FilePath -> App (Map String CategoryConstraints)
readMetadata potentialCategory = do
	explain $ "Considering potential category " <> potentialCategory
	metaE <- parseFile (GamesProcessed potentialCategory) metadataFilename
	listE <- parseFile (GamesProcessed potentialCategory) namesFilename
	case (metaE, listE) of
		(Left e, _) -> do
			explain $ printf "Skipping %s due to error while processing %s: %s" potentialCategory metadataFilename e
			case listE of
				Left e' -> explain $ printf "Additionally, encountered an error while processing %s: %s" namesFilename e'
				_ -> pure ()
			pure M.empty
		(_, Left e) -> do
			explain $ printf "Skipping %s due to error while processing %s: %s" potentialCategory namesFilename e
			pure M.empty
		(_, Right []) -> do
			explain $ printf "Skipping %s because %s claims there are no games live, which is very suspicious." potentialCategory namesFilename
			pure M.empty
		(Right meta, Right list_) -> do
			let list = reverse list_
			smallest:_ <- if isSorted list
				then pure list
				else do
					liftIO . hPutStr stderr $ ""
						++ "WARNING: The list of live filenames is not sorted. The behavior of this\n"
						++ "program was designed under the assumption that newer games have a larger file\n"
						++ "name and get added at the front of the list, so it may not behave as desired.\n"
						++ "The problematic category is " ++ potentialCategory ++ ".\n"
					pure (sort list)
			pure $ M.singleton potentialCategory CategoryConstraints
				{ minFilename = smallest
				, otherFilenames = cmetaGatherFiles meta
				}

listDirectoryM :: Directory -> App [FilePath]
listDirectoryM dir = do
	nm <- directoryNameM dir
	explain $ "Listing files in " <> nm
	liftIO $ listDirectory nm

parseFile :: FromJSON a => Directory -> FilePath -> App (Either String a)
parseFile dir fp = do
	nm <- fileNameM dir fp
	explain $ "Parsing " <> nm
	liftIO $ catch (eitherDecodeFileStrict nm) mkErr
	where
	mkErr :: IOException -> IO (Either String a)
	mkErr = pure . Left . displayException

explain :: String -> App ()
explain = whenVerbose . liftIO . putStrLn

directoryNameM :: Directory -> App FilePath
directoryNameM dir = asks (\env -> absDirectoryName (eDataDir env) dir)

fileNameM :: Directory -> FilePath -> App FilePath
fileNameM dir fp = asks (\env -> absFileName (eDataDir env) dir fp)

whenVerbose :: App () -> App ()
whenVerbose act = do
	verb <- asks eVerbose
	when verb act

data Env = Env
	{ eVerbose :: Bool
	, eDryRun :: Bool
	, eDataDir :: FilePath
	}

type App = ReaderT Env IO

runApp :: Args -> App a -> IO ()
runApp args app = do
	dir <- nsDataDir
	() <$ runReaderT app Env
		{ eVerbose = aVerbose args
		, eDryRun = aDryRun args
		, eDataDir = dir
		}

cmetaGatherFiles :: CategoryMetadata -> Set FilePath
cmetaGatherFiles meta = foldMap (\f -> cmetricGatherFiles (f meta)) [cmBest, cmLatest, cmCumulative]

cmetricGatherFiles :: CategoryMetrics -> Set FilePath
cmetricGatherFiles metric = foldMap (\f -> lmGatherFiles (f metric)) [cmVirusesKilled, cmFramesToWin, cmFramesToLoss, cmFrames]

lmGatherFiles :: Foldable f => f LevelMetric -> Set FilePath
lmGatherFiles = foldMap (S.singleton . lmSource)

isSorted :: Ord a => [a] -> Bool
isSorted (x:xs@(x':_)) = x < x' && isSorted xs
isSorted _ = True

data Args = Args
	{ aVerbose :: Bool
	, aDryRun :: Bool
	, aHelp :: Bool
	, aUnrecognized :: [String]
	} deriving (Eq, Ord, Read, Show)

instance Semigroup Args where
	args <> args' = Args
		{ aVerbose = aVerbose args || aVerbose args'
		, aDryRun = aDryRun args || aDryRun args'
		, aHelp = aHelp args || aHelp args'
		, aUnrecognized = aUnrecognized args ++ aUnrecognized args'
		}

instance Monoid Args where
	mempty = Args
		{ aVerbose = False
		, aDryRun = False
		, aHelp = False
		, aUnrecognized = []
		}

parseArgs :: [String] -> Args
parseArgs = foldMap \case
	"--verbose" -> mempty { aVerbose = True }
	"--dry-run" -> mempty { aDryRun = True }
	"--help" -> mempty { aHelp = True }
	arg@('-':'-':_) -> mempty { aUnrecognized = [arg] }
	'-':chars -> foldMap parseSingleCharArg chars
	arg -> mempty { aUnrecognized = [arg] }

parseSingleCharArg :: Char -> Args
parseSingleCharArg = \case
	'v' -> mempty { aVerbose = True }
	'n' -> mempty { aDryRun = True }
	'h' -> mempty { aHelp = True }
	c -> mempty { aUnrecognized = [['-', c]] }

-- this definition comes last because it's got especially tricky syntax that
-- highlighters can get tripped up on, and then be in a wrong state for the
-- rest of the file
usage :: ExitCode -> Handle -> IO a
usage code h = do
	nm <- getProgName
	dir <- nsDataDir
	hPrintf h "\
		\USAGE: %s [FLAGS]\n\
		\\n\
		\Move games that are no longer going to be used from the processed directory to\n\
		\the archive directory. These directories are stored under\n\
		\%s\n\
		\\n\
		\\t-n, --dry-run          Don't move anything, just report the plan.\n\
		\\t-v, --verbose          Explain the thought process.\n\
		\\t-h, --help             Print this message and stop.\n\
		\"
		nm dir
	exitWith code
