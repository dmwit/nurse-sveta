module Ms.Mendel
	( module Ms.Mendel
	, module Nurse.Sveta.Genome
	, module Nurse.Sveta.Tomcats
	, module Nurse.Sveta.Util
	) where

import GHC.Generics
import Nurse.Sveta.Files
import Nurse.Sveta.Genome
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Util

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

data GenomeConfig = GenomeConfig
	{ gcInitialPatterns :: Int
	, gcMaxPatterns :: Int
	} deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON GenomeConfig where parseJSON = genericParseJSON (dashParseJSONOptions "GenomeConfig" "gc")
instance ToJSON GenomeConfig where toEncoding = genericToEncoding (dashParseJSONOptions "GenomeConfig" "gc")

data MsMendelConfig = MsMendelConfig
	{ mmcInitialEvaluationThreads :: Int
	, mmcInitialEvolutionThreads :: Int
	, mmcInitialPopulation :: Int
	, mmcPillCycleLength :: Int
	, mmcEvaluationRateLimit :: Int
	, mmcRunsPerGeneration :: Int
	, mmcSurvivors :: Int
	, mmcBreeders :: Int
	, mmcMutators :: Int
	, mmcOffspring :: Int
	, mmcGeneReplacements :: Int
	, mmcGeneDeletions :: Int
	, mmcGeneAdditions :: Int
	, mmcPatternToggles :: Int
	, mmcScoreAdjustments :: Int
	, mmcScoreAdjustmentVariance :: Float
	, mmcScoreResets :: Int
	, mmcBulkPatternToggles :: Int
	, mmcTypicalPatternToggleBatchSize :: Float
	, mmcMaxLevel :: Int
	, mmcGenomeConfig :: HashMap ConvolutionSize GenomeConfig
	, mmcGeneMirroring :: Bool
	, mmcMaxPillsPerKill :: Int
	, mmcLogDirectory :: Maybe FilePath
	, mmcInitialScoreAdjustments :: Int
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

savePopulation :: FilePath -> Vector Individual -> Int -> IO ()
savePopulation dir pop generation = do
	saveAtomically dir (show generation <.> "json") (RecordOfVectors (iSpec <$> pop))
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
	= MissingGeneration FilePath
	| MissingPopulation FilePath
	| Corrupt FilePath String
	deriving (Eq, Ord, Read, Show)

loadPopulation_ :: MsMendelConfig -> FilePath -> IO (Int, Vector Individual)
loadPopulation_ mmc dir = reflectError $ loadPopulation mmc dir

loadPopulationAsSpecs_ :: FilePath -> IO (Int, Vector IndividualSpec)
loadPopulationAsSpecs_ = reflectError . loadPopulationAsSpecs

loadGeneration_ :: MsMendelConfig -> FilePath -> Int -> IO (Vector Individual)
loadGeneration_ mmc dir generation = reflectError $ loadGeneration mmc dir generation

loadGenerationAsSpecs_ :: FilePath -> Int -> IO (Vector IndividualSpec)
loadGenerationAsSpecs_ dir generation = reflectError $ loadGenerationAsSpecs dir generation

loadPopulation :: MsMendelConfig -> FilePath -> IO (Either LoadingError (Int, Vector Individual))
loadPopulation mmc dir = loadPopulationAsSpecs dir >>= traverse (popFromSpecs mmc)

loadPopulationAsSpecs :: FilePath -> IO (Either LoadingError (Int, Vector IndividualSpec))
loadPopulationAsSpecs dir = do
	let generationFilename = dir </> "latest.json"
	handle (missing (MissingGeneration generationFilename)) do
		bsGeneration <- LBS.readFile generationFilename
		handle (corrupt generationFilename) do
			generation <- throwDecode bsGeneration
			fmap ((,) generation) <$> loadGenerationAsSpecs dir generation

loadGeneration :: MsMendelConfig -> FilePath -> Int -> IO (Either LoadingError (Vector Individual))
loadGeneration mmc dir generation = loadGenerationAsSpecs dir generation >>= popFromSpecs mmc

loadGenerationAsSpecs :: FilePath -> Int -> IO (Either LoadingError (Vector IndividualSpec))
loadGenerationAsSpecs dir generation = do
	let specsFilename = dir </> show generation <.> "json"
	handle (missing (MissingPopulation specsFilename)) do
		bsSpecs <- LBS.readFile specsFilename
		handle (corrupt specsFilename) do
			RecordOfVectors specs <- throwDecode bsSpecs
			pure (Right specs)

popFromSpecs :: Traversable t => MsMendelConfig -> t (Vector IndividualSpec) -> IO (t (Vector Individual))
popFromSpecs mmc = traverse (traverse (iFromSpec (mmcGeneMirroring mmc)))

corrupt :: FilePath -> AesonException -> IO (Either LoadingError a)
corrupt fp (AesonException e) = pure (Left (Corrupt fp e))

missing :: LoadingError -> IOException -> IO (Either LoadingError a)
missing failure e = if isDoesNotExistError e then pure (Left failure) else throw e

breed :: MsMendelConfig -> GenIO -> Vector Individual -> IO (Vector Individual)
breed mmc rng pop
	| V.length pop < 2 = pure V.empty
	| otherwise = V.replicateM (mmcOffspring mmc) do
		(ind, ind') <- chooseTwo
		HM.traverseWithKey (\cs cfg -> liftJ2 (breedGenome cfg) (iGenome mmc ind cs) (iGenome mmc ind' cs)) (mmcGenomeConfig mmc)
	where
	chooseTwo = do
		[a, b] <- replicateM 2 (uniformVI' rng pop)
		if a == b then chooseTwo else pure (pop V.! a, pop V.! b)
	breedGenome cfg g g' = do
		shuffledIndices <- uniformShuffle ((Left <$> V.generate (gSize g) id) <> (Right <$> V.generate (gSize g') id)) rng
		len <- min (gcMaxPatterns cfg) . (1+) <$> uniformVI' rng shuffledIndices
		let (indices, indices') = V.partitionWith id (V.take len shuffledIndices)
		liftJ2 gAppend (gIndices g (V.toList indices)) (gIndices g' (V.toList indices'))

mutate :: MsMendelConfig -> GenIO -> Vector Individual -> IO (Vector Individual)
mutate _mmc _rng pop | all ((0==) . iSize) pop = pure V.empty -- this should never happen
mutate mmc rng pop = do
	ins <- V.replicateM (mmcGeneReplacements mmc) replaceGene
	del <- V.replicateM (mmcGeneDeletions mmc) deleteGene
	pat <- V.replicateM (mmcPatternToggles mmc) togglePattern
	adj <- V.replicateM (mmcScoreAdjustments mmc) adjustScore
	res <- V.replicateM (mmcScoreResets mmc) resetScore
	blk <- V.replicateM (mmcBulkPatternToggles mmc) bulkPatternToggle
	pure $ mconcat [ins, del, pat, adj, res, blk]
	where
	replaceGene = onGene rng pop \cs pat sz g -> liftJ2 gAppend
		(gIndices g $ [0..pat-1] ++ [pat+1..sz-1])
		(newJeffreysGenome mmc rng cs 1)
	deleteGene = onGene rng pop \_cs pat sz g -> gIndices g $ [0..pat-1] ++ [pat+1..sz-1]
	togglePattern = onGeneClone rng pop \cs pat _sz g -> do
		chan <- uniformV' rng allChannels
		x <- uniformIndex (csWidth cs)
		y <- uniformIndex (csHeight cs)
		case chan of
			Left  color -> gSetColorPattern g pat color x y . not $ gGetColorPattern g pat color x y
			Right shape -> gSetShapePattern g pat shape x y . not $ gGetShapePattern g pat shape x y
	adjustScore = onGeneClone rng pop \_cs pat _sz g -> do
		delta <- standard rng
		gSetPatternScore g pat . tweakScore mmc delta $ gGetPatternScore g pat
	resetScore = onGeneClone rng pop \_cs pat _sz g -> gSetPatternScore g pat 0
	bulkPatternToggle = onGeneClone rng pop \cs pat _sz g -> do
		pat' <- newJeffreysGenome mmc rng cs 1
		let loop = do
		    	x <- uniformIndex (csWidth cs)
		    	y <- uniformIndex (csHeight cs)
		    	for_ allColorsWithSentinels \c -> gSetColorPattern g pat c x y (gGetColorPattern pat' 0 c x y)
		    	for_ allShapesWithSentinels \s -> gSetShapePattern g pat s x y (gGetShapePattern pat' 0 s x y)
		    	n <- uniformFloat01M rng
		    	when (n > pDone) loop
		    -- this calculation isn't exactly correct because we make no
		    -- attempt to choose unique locations each time, but meh, close
		    -- enough
		    pDone = recip (mmcTypicalPatternToggleBatchSize mmc)
		loop
	uniformIndex n = uniformRM (0, n-1) rng

-- use the Jeffreys prior for Bernoulli distributions, β(½,½), to choose the
-- Bernoulli parameter
newJeffreysGenome :: MsMendelConfig -> GenIO -> ConvolutionSize -> Int -> IO Genome
newJeffreysGenome mmc rng cs n = newGenome (mmcGeneMirroring mmc) cs n . realToFrac =<< beta 0.5 0.5 rng

newJeffreysIndividual :: MsMendelConfig -> GenIO -> IO Individual
newJeffreysIndividual mmc rng = HM.traverseWithKey
	(\cs -> newJeffreysGenome mmc rng cs . gcInitialPatterns)
	(mmcGenomeConfig mmc)

-- addGenes is not part of mutate because addGenes can only work if there's
-- some genome below max size, while mutate can only work if there's some
-- genome above zero size.
addGenes :: MsMendelConfig -> GenIO -> Vector Individual -> IO (Vector Individual)
addGenes mmc _rng pop | all ((sum (gcMaxPatterns <$> mmcGenomeConfig mmc)==) . iSize) pop = pure V.empty -- this should never happen
addGenes mmc rng pop = V.replicateM (mmcGeneAdditions mmc) addGene where
	convolutionSizes = V.fromList (HM.toList (mmcGenomeConfig mmc))
	addGene = do
		ind <- uniformV' rng pop
		(cs, gc) <- uniformV' rng convolutionSizes
		g <- iGenome mmc ind cs
		if gSize g < gcMaxPatterns gc
			then pure . flip (HM.insert cs) ind =<< gAppend g =<< newJeffreysGenome mmc rng cs 1
			else addGene

tweakScore :: MsMendelConfig -> Double -> Float -> Float
tweakScore mmc delta score = tanh (mmcScoreAdjustmentVariance mmc * realToFrac delta + atanh (min 0.9999999 (max (-0.9999999) score)))

onGene :: GenIO -> Vector Individual -> (ConvolutionSize -> Int -> Int -> Genome -> IO Genome) -> IO Individual
onGene rng pop f = do
	ind <- uniformV' rng pop
	if iSize ind == 0 then onGene rng pop f else do
		(cs, pat, sz, g) <- uniformV' rng . V.fromList $
			[ (cs, pat, sz, g)
			| (cs, g) <- HM.toList ind
			, let sz = gSize g
			, pat <- [0..sz-1]
			]
		flip (HM.insert cs) ind <$> f cs pat sz g

onGeneClone :: GenIO -> Vector Individual -> (ConvolutionSize -> Int -> Int -> Genome -> IO ()) -> IO Individual
onGeneClone rng pop f = onGene rng pop \cs pat sz g_ -> do
	g <- gClone g_
	g <$ f cs pat sz g

iGenome :: MsMendelConfig -> Individual -> ConvolutionSize -> IO Genome
iGenome mmc ind cs = case HM.lookup cs ind of
	Just g -> pure g
	Nothing -> newGenome (mmcGeneMirroring mmc) cs 0 0

iSize :: Individual -> Int
iSize = sum . fmap gSize

allChannels :: Vector (Either (WithSentinels Color) (WithSentinels Shape))
allChannels = fmap Left allColorSentinels <> fmap Right allShapeSentinels

allColorSentinels :: Vector (WithSentinels Color)
allColorSentinels = addSentinels [minBound..maxBound]

allShapeSentinels :: Vector (WithSentinels Shape)
allShapeSentinels = addSentinels [Virus, Disconnected, East, West] -- North, South = Disconnected

addSentinels :: [a] -> Vector (WithSentinels a)
addSentinels as = V.fromList $ map NonSentinel as ++ [EmptySentinel, OutOfBoundsSentinel]
