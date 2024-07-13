module Nurse.Sveta.Files (
	module Nurse.Sveta.Files,
	module Paths_nurse_sveta,
	module System.Directory,
	module System.FilePath,
	) where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import Paths_nurse_sveta
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Error

nsAppName :: FilePath
nsAppName = "nurse-sveta"

nsDataDir :: IO FilePath
nsDataDir = getXdgDirectory XdgData nsAppName

nsRuntimeDir :: IO FilePath
nsRuntimeDir = (</> nsAppName) <$> do
	fromEnv <- lookupEnv "XDG_RUNTIME_DIR"
	case fromEnv of
		Just fp -> pure fp
		Nothing -> getTemporaryDirectory

-- | Given a string, find a valid filename (i.e. one that does not contain @/@,
-- @\\@, or @:@) that decodes to that string. See 'dirDecode' for details on
-- what that means.
--
-- This isn't really a comprehensive way to avoid invalid file names,
-- especially on Windows, which has a large collection of restrictions on
-- filenames. Perhaps some day I will care about making that happen. For now,
-- it covers the easiest ways of screwing up.
dirEncode :: String -> FilePath
dirEncode "" = "q"
dirEncode s = go False s where
	go active (c:s) = case c of
		'q'  -> "q"  ++ go True  s
		'/'  -> "qx" ++ go False s
		'\\' -> "qg" ++ go False s
		':'  -> "qj" ++ go False s
		c | active && c `elem` ("gjxz" :: String) -> "qz" ++ c : go False s
		  | otherwise -> c : go False s
	go active [] = ['q' | active]

-- | Identify maximal-length chunks of @q@s. For each one, chop off one @q@,
-- then replace the next character after the chunk depending on what it is:
--
-- * @g@ is replaced with a backslash
-- * @j@ is replaced with a colon
-- * @x@ is replaced with a slash
-- * @z@ or the end of the filename is replaced with the empty string
-- * other characters are replaced by a @q@ followed by that character (i.e.
--   the full effect is that nothing changes around this chunk -- no @q@s
--   actually end up being deleted, and the terminator is kept as-is)
dirDecode :: FilePath -> String
dirDecode = normal where
	normal ('q':s) = q s
	normal (c:s) = c:normal s
	normal [] = []

	q (c:s) = case c of
		'g' -> '\\':normal s
		'j' -> ':' :normal s
		'q' -> 'q' :q s
		'x' -> '/' :normal s
		'z' ->      normal s
		_ -> 'q':c :normal s
	q [] = ""

data Directory
	= GamesPending
	| GamesProcessed FilePath
	| GamesArchive FilePath
	| GamesParseError
	| Weights
	| Logging
	deriving (Eq, Ord, Read, Show)

relFileName :: Directory -> FilePath -> FilePath
relFileName dir fp = case dir of
	GamesPending            -> "games" </> "pending" </> fp
	GamesProcessed category -> "games" </> "processed" </> category </> fp
	GamesArchive category   -> "games" </> "archive" </> category </> fp
	GamesParseError         -> "games" </> "parse-error" </> fp
	Weights                 -> "weights" </> fp
	Logging                 -> "wandb" </> fp

relDirectoryName :: Directory -> FilePath
relDirectoryName dir = relFileName dir ""

absFileName :: FilePath -> Directory -> FilePath -> FilePath
absFileName root dir fp = root </> relFileName dir fp

absDirectoryName :: FilePath -> Directory -> FilePath
absDirectoryName root dir = root </> relDirectoryName dir

relPrepareFile :: Directory -> FilePath -> IO FilePath
relPrepareFile dir fp = do
	root <- nsDataDir
	absPrepareFile root dir fp

absPrepareFile :: FilePath -> Directory -> FilePath -> IO FilePath
absPrepareFile root dir fp = (</> fp) <$> absPrepareDirectory root dir

relPrepareDirectory :: Directory -> IO FilePath
relPrepareDirectory dir = nsDataDir >>= flip absPrepareDirectory dir

absPrepareDirectory :: FilePath -> Directory -> IO FilePath
absPrepareDirectory root dir = subdir <$ createDirectoryIfMissing True subdir where
	subdir = absDirectoryName root dir

latestFilename, metadataFilename, runtimeFilename, namesFilename :: FilePath
latestFilename = "latest.json"
metadataFilename = "meta.json"
namesFilename = "files.json"
runtimeFilename = "runtime"

rawEncodeFileLoop :: ToJSON a => FilePath -> a -> IO ()
rawEncodeFileLoop fp a = catch
	(encodeFile fp a)
	(\e -> if isAlreadyInUseError e then threadDelay 1000 >> rawEncodeFileLoop fp a else throwIO e)

absEncodeFileLoop :: ToJSON a => FilePath -> Directory -> FilePath -> a -> IO ()
absEncodeFileLoop root dir fp = rawEncodeFileLoop (absFileName root dir fp)

relEncodeFileLoop :: ToJSON a => Directory -> FilePath -> a -> IO ()
relEncodeFileLoop dir fp a = do
	root <- nsDataDir
	absEncodeFileLoop root dir fp a

rawDecodeFileLoop :: FromJSON a => FilePath -> IO (Maybe a)
rawDecodeFileLoop fp = catch (decodeFileStrict' fp) $ \e -> if
	| isAlreadyInUseError e -> threadDelay 1000 >> rawDecodeFileLoop fp
	| isDoesNotExistError e -> pure Nothing
	| otherwise -> throwIO e

absDecodeFileLoop :: FromJSON a => FilePath -> Directory -> FilePath -> IO (Maybe a)
absDecodeFileLoop root dir fp = rawDecodeFileLoop (absFileName root dir fp)

relDecodeFileLoop :: FromJSON a => Directory -> FilePath -> IO (Maybe a)
relDecodeFileLoop dir fp = do
	root <- nsDataDir
	absDecodeFileLoop root dir fp

relocate :: FilePath -> FilePath -> Directory -> Directory -> IO ()
relocate root fp from to = do
	path <- absPrepareFile root to fp
	renameFile (absFileName root from fp) path
