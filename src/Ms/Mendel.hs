module Ms.Mendel
	( module Ms.Mendel
	, module Ms.Mendel.Cairo
	, module Ms.Mendel.Population
	, module Ms.Mendel.Widget
	, module Nurse.Sveta.Util
	) where

import Control.Monad.Except
import GHC.Generics
import Ms.Mendel.Cairo
import Ms.Mendel.Population
import Ms.Mendel.Widget
import Nurse.Sveta.Util

import qualified Data.ByteString.Lazy.Char8 as LBS

data MsMendelConfig = MsMendelConfig
	{ mmcMaxPillsPerKill :: Int
	, mmcLogDirectory :: Maybe FilePath
	} deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON MsMendelConfig where parseJSON = genericParseJSON (dashParseJSONOptions "MsMendelConfig" "mmc")
instance ToJSON MsMendelConfig where toEncoding = genericToEncoding (dashParseJSONOptions "MsMendelConfig" "mmc")

dashParseJSONOptions :: String -> String -> Options
dashParseJSONOptions typeName prefix = defaultOptions
	{ fieldLabelModifier = \fieldName -> case stripPrefix prefix fieldName of
		Just s -> drop 1 [c' | c <- s, c' <- ['-' | isUpper c] ++ [toLower c]]
		Nothing -> error $ "unexpected field name " ++ show fieldName ++ " in parseJSON @" ++ typeName
	, allowOmittedFields = False
	, rejectUnknownFields = True
	}

basedir :: XdgDirectory -> IO FilePath
basedir dir = getXdgDirectory dir "ms-mendel"

loadFromConfiguration :: FromJSON a => String -> IO a
loadFromConfiguration nm = do
	dir <- basedir XdgConfig
	val <- eitherDecodeFileStrict (dir </> nm <.> "json")
	either fail pure val

loadConfiguration :: IO MsMendelConfig
loadConfiguration = loadFromConfiguration "config"

savePopulation :: FilePath -> Population Disk -> Int -> IO ()
savePopulation dir pop generation = do
	saveAtomically dir (show generation <.> "json") pop
	saveAtomically dir "latest.json" generation

saveAtomically :: ToJSON a => FilePath -> FilePath -> a -> IO ()
saveAtomically dir nm a = do
	encodeFile (dir </> "." ++ nm) a
	renameFile (dir </> "." ++ nm) (dir </> nm)

makeLogger :: MsMendelConfig -> String -> IO (String -> IO ())
makeLogger mmc threadName = do
	mdir <- for (mmcLogDirectory mmc) \case
		"$CONFIG" -> basedir XdgConfig
		"$DATA" -> basedir XdgData
		"$HOME" -> getHomeDirectory
		"~" -> getHomeDirectory
		'$':'C':'O':'N':'F':'I':'G':'/':rest -> (</>rest) <$> basedir XdgConfig
		'$':'D':'A':'T':'A':'/':rest -> (</>rest) <$> basedir XdgData
		'$':'H':'O':'M':'E':'/':rest -> (</>rest) <$> getHomeDirectory
		'~':'/':rest -> (</>rest) <$> getHomeDirectory
		'/':absolute -> pure ('/':absolute)
		relative -> do
			printf "WARNING: logging to relative directory %s\n" relative
			printf "\tcurrent working directory is %s\n" =<< getCurrentDirectory
			printf "\tyou might want to consider using $CONFIG, $DATA, or $HOME to construct an absolute path instead\n"
			pure relative
	startTime <- getCurrentTime
	threadId <- myThreadId
	mh <- for mdir \dir -> do
		createDirectoryIfMissing True dir
		openFile (dir </> printf "%s-%s-%s" threadName (show startTime) (show threadId) <.> "txt") WriteMode
	let stdoutPrefix = printf "%s@%s [%s]: " threadName (show startTime) (show threadId)
	pure \s -> do
		putFlush (stdoutPrefix ++ s) stdout
		for_ mh (putFlush s)
	where
	putFlush s h = hPutStrLn h s >> hFlush h

data LoadingError
	= Missing FilePath String
	| Corrupt FilePath String
	deriving (Eq, Ord, Read, Show)

-- | data dir
loadPopulation_ :: FilePath -> IO (Population Disk)
loadPopulation_ = reflectError . loadPopulation

-- | data dir
loadPopulation :: FilePath -> IO (Either LoadingError (Population Disk))
loadPopulation dir = runExceptT do
	generation <- loadJSON (dir </> "latest.json") "generation"
	let populationFilename = dir </> show generation <.> "json"
	population <- loadJSON populationFilename "population"
	when (generation /= pdGeneration population) $
		throwError (Corrupt populationFilename "filename/generation mismatch")
	pure population

-- | config dir
loadPatterns_ :: FilePath -> IO (PatternGroups Authoring)
loadPatterns_ = reflectError . loadPatterns

-- | config dir
loadPatterns :: FilePath -> IO (Either LoadingError (PatternGroups Authoring))
loadPatterns dir = runExceptT (loadJSON (dir </> "patterns.json") "patterns")

loadJSON :: FromJSON a => FilePath -> String -> ExceptT LoadingError IO a
loadJSON path ty = ExceptT $ handle (missing path ty) do
	bs <- LBS.readFile path
	handle (corrupt path) (Right <$> throwDecode bs)

corrupt :: FilePath -> AesonException -> IO (Either LoadingError a)
corrupt fp (AesonException e) = pure (Left (Corrupt fp e))

missing :: FilePath -> String -> IOException -> IO (Either LoadingError a)
missing fp ty e = if isDoesNotExistError e then pure (Left (Missing fp ty)) else throw e
