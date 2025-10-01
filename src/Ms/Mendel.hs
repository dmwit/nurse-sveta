module Ms.Mendel
	( module Ms.Mendel
	, module Nurse.Sveta.Genome
	, module Nurse.Sveta.Tomcats
	, module Nurse.Sveta.Util
	) where

import Data.Bifunctor
import GHC.Generics
import Nurse.Sveta.Files
import Nurse.Sveta.Genome
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Util

import qualified Data.Aeson.KeyMap as KM
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
	, mmcSinglePatternScoreAdjustments :: Int
	, mmcSinglePatternScoreAdjustmentVariance :: Float
	, mmcAllPatternsScoreAdjustments :: Int
	, mmcAllPatternsScoreAdjustmentVariance :: Float
	, mmcScoreResets :: Int
	, mmcBulkPatternToggles :: Int
	, mmcTypicalPatternToggleBatchSize :: Float
	, mmcFramePenaltyAdjustments :: Int
	, mmcFramePenaltyVariance :: Float
	, mmcFramePenaltyResets :: Int
	, mmcMaxLevel :: Int
	, mmcGenomeConfig :: HashMap ConvolutionSize GenomeConfig
	, mmcGeneMirroring :: Bool
	, mmcMaxPillsPerKill :: Int
	, mmcLogDirectory :: Maybe FilePath
	, mmcInitialSinglePatternScoreAdjustments :: Int
	, mmcInitialFrameScore :: Float
	} deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON MsMendelConfig where parseJSON = genericParseJSON (dashParseJSONOptions "MsMendelConfig" "mmc")
instance ToJSON MsMendelConfig where toEncoding = genericToEncoding (dashParseJSONOptions "MsMendelConfig" "mmc")

instance FromJSON a => FromJSON (IndividualOf a) where parseJSON = genericParseJSON (dashParseJSONOptions "Individual" "i")
instance ToJSON   a => ToJSON (IndividualOf a) where
	toEncoding = genericToEncoding (dashParseJSONOptions "Individual" "i")
	toJSON = genericToJSON (dashParseJSONOptions "Individual" "i")

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

iSpecToJSON :: Vector IndividualSpec -> Value
iSpecToJSON spec = case toJSON (RecordOfVectors spec) of
	Object o -> case KM.lookup k o of
		Just (Array gs) -> Object (KM.insert k (toJSON (RecordOfVectors gs)) o)
		_ -> error "TODO: handle an object that's missing genes in iSpecToJSON"
	_ -> error "TODO: handle a vector of specs that isn't serialized to an object"
	where k = "genes"

iSpecParseJSON :: Value -> Parser (Vector IndividualSpec)
iSpecParseJSON (Object o) = case KM.lookup k o of
	Just v -> do
		RecordOfVectors v' <- parseJSON v
		RecordOfVectors specs <- parseJSON (Object (KM.insert k (Array v') o))
		pure specs
	_ -> error "TODO: handle an object that's missing genes in iSpecParseJSON"
	where k = "genes"
iSpecParseJSON other = typeMismatch "object with keys genes and frame-score" other

savePopulation :: FilePath -> Vector Individual -> Int -> IO ()
savePopulation dir pop generation = do
	saveAtomically dir (show generation <.> "json") (iSpecToJSON (iSpec <$> pop))
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

loadPopulation_ :: FilePath -> IO (Int, Vector Individual)
loadPopulation_ dir = reflectError $ loadPopulation dir

loadPopulationAsSpecs_ :: FilePath -> IO (Int, Vector IndividualSpec)
loadPopulationAsSpecs_ = reflectError . loadPopulationAsSpecs

loadGeneration_ :: FilePath -> Int -> IO (Vector Individual)
loadGeneration_ dir generation = reflectError $ loadGeneration dir generation

loadGenerationAsSpecs_ :: FilePath -> Int -> IO (Vector IndividualSpec)
loadGenerationAsSpecs_ dir generation = reflectError $ loadGenerationAsSpecs dir generation

loadPopulation :: FilePath -> IO (Either LoadingError (Int, Vector Individual))
loadPopulation dir = loadPopulationAsSpecs dir >>= traverse popFromSpecs

loadPopulationAsSpecs :: FilePath -> IO (Either LoadingError (Int, Vector IndividualSpec))
loadPopulationAsSpecs dir = do
	let generationFilename = dir </> "latest.json"
	handle (missing (MissingGeneration generationFilename)) do
		bsGeneration <- LBS.readFile generationFilename
		handle (corrupt generationFilename) do
			generation <- throwDecode bsGeneration
			fmap ((,) generation) <$> loadGenerationAsSpecs dir generation

loadGeneration :: FilePath -> Int -> IO (Either LoadingError (Vector Individual))
loadGeneration dir generation = loadGenerationAsSpecs dir generation >>= popFromSpecs

loadGenerationAsSpecs :: FilePath -> Int -> IO (Either LoadingError (Vector IndividualSpec))
loadGenerationAsSpecs dir generation = do
	let specsFilename = dir </> show generation <.> "json"
	handle (missing (MissingPopulation specsFilename)) do
		bsSpecs <- LBS.readFile specsFilename
		handle (corrupt specsFilename) do
			specs <- throwDecode bsSpecs
			pure $ bimap (Corrupt specsFilename) id (parseEither iSpecParseJSON specs)

popFromSpecs :: Traversable t => t (Vector IndividualSpec) -> IO (t (Vector Individual))
popFromSpecs = traverse (traverse iFromSpec)

corrupt :: FilePath -> AesonException -> IO (Either LoadingError a)
corrupt fp (AesonException e) = pure (Left (Corrupt fp e))

missing :: LoadingError -> IOException -> IO (Either LoadingError a)
missing failure e = if isDoesNotExistError e then pure (Left failure) else throw e

breed :: MsMendelConfig -> GenIO -> Vector Individual -> IO (Vector Individual)
breed mmc rng pop
	| V.length pop < 2 = pure V.empty
	| otherwise = V.replicateM (mmcOffspring mmc) do
		(ind, ind') <- chooseTwo
		genes <- HM.traverseWithKey (\cs cfg -> liftJ2 (breedGenome cfg) (iGenome mmc ind cs) (iGenome mmc ind' cs)) (mmcGenomeConfig mmc)
		frameScore <- uniformV' rng . V.fromList . map iFrameScore $ [ind, ind']
		pure Individual
			{ iGenes = genes
			, iFrameScore = frameScore
			}
	where
	chooseTwo = do
		[a, b] <- replicateM 2 (uniformVI' rng pop)
		if a == b then chooseTwo else pure (pop V.! a, pop V.! b)
	breedGenome cfg g g' = do
		shuffledIndices <- uniformShuffle ((Left <$> V.generate (gSize g) id) <> (Right <$> V.generate (gSize g') id)) rng
		len <- min (gcMaxPatterns cfg) . (1+) <$> uniformVI' rng shuffledIndices
		let (indices, indices') = V.partitionWith id (V.take len shuffledIndices)
		-- this sort of accidentally chooses randomly between the mirroring
		-- settings of the two genomes, because you're equally likely to get
		-- them in either order out of chooseTwo. happy accident
		liftJ2 gAppend (gIndices g (V.toList indices)) (gIndices g' (V.toList indices'))

mutate :: MsMendelConfig -> GenIO -> Vector Individual -> IO (Vector Individual)
mutate _mmc _rng pop | all ((0==) . iSize) pop = pure V.empty -- this should never happen
mutate mmc rng pop = do
	ins <- V.replicateM (mmcGeneReplacements mmc) replaceGene
	del <- V.replicateM (mmcGeneDeletions mmc) deleteGene
	pat <- V.replicateM (mmcPatternToggles mmc) togglePattern
	adj <- V.replicateM (mmcSinglePatternScoreAdjustments mmc) adjustScore
	nrm <- V.replicateM (mmcAllPatternsScoreAdjustments mmc) adjustAllScores
	res <- V.replicateM (mmcScoreResets mmc) resetScore
	blk <- V.replicateM (mmcBulkPatternToggles mmc) bulkPatternToggle
	frm <- V.replicateM (mmcFramePenaltyAdjustments mmc) framePenaltyAdjustment
	frs <- V.replicateM (mmcFramePenaltyResets mmc) framePenaltyReset
	pure $ mconcat [ins, del, pat, adj, nrm, res, blk, frm, frs]
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
	adjustAllScores = do
		i <- uniformV' rng pop >>= iClone
		let var = mmcAllPatternsScoreAdjustmentVariance mmc / fromIntegral (iSize i)
		i <$ for_ i \g -> gTweakPatternScores g var
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
	framePenaltyAdjustment = do
		i <- uniformV' rng pop >>= iClone
		mag <- standard rng
		pure i { iFrameScore = exp (mmcFramePenaltyVariance mmc * realToFrac mag) * iFrameScore i }
	framePenaltyReset = do
		i <- uniformV' rng pop >>= iClone
		pure i { iFrameScore = mmcInitialFrameScore mmc }
	uniformIndex n = uniformRM (0, n-1) rng

-- use the Jeffreys prior for Bernoulli distributions, β(½,½), to choose the
-- Bernoulli parameter
newJeffreysGenome :: MsMendelConfig -> GenIO -> ConvolutionSize -> Int -> IO Genome
newJeffreysGenome mmc rng cs n = newGenome (mmcGeneMirroring mmc) cs n . realToFrac =<< beta 0.5 0.5 rng

newJeffreysIndividual :: MsMendelConfig -> GenIO -> IO Individual
newJeffreysIndividual mmc rng = mmcIndividual mmc <$> HM.traverseWithKey
	(\cs -> newJeffreysGenome mmc rng cs . gcInitialPatterns)
	(mmcGenomeConfig mmc)

mmcIndividual :: MsMendelConfig -> HashMap ConvolutionSize Genome -> Individual
mmcIndividual mmc gs = Individual
	{ iGenes = gs
	, iFrameScore = mmcInitialFrameScore mmc
	}

iInsert :: Individual -> ConvolutionSize -> Genome -> Individual
iInsert ind cs g = ind { iGenes = HM.insert cs g (iGenes ind) }

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
			then pure . iInsert ind cs =<< gAppend g =<< newJeffreysGenome mmc rng cs 1
			else addGene

tweakScore :: MsMendelConfig -> Double -> Float -> Float
tweakScore mmc delta score = tanh (mmcSinglePatternScoreAdjustmentVariance mmc * realToFrac delta + atanh (min 0.9999999 (max (-0.9999999) score)))

onGene :: GenIO -> Vector Individual -> (ConvolutionSize -> Int -> Int -> Genome -> IO Genome) -> IO Individual
onGene rng pop f = do
	ind <- uniformV' rng pop
	if iSize ind == 0 then onGene rng pop f else do
		(cs, pat, sz, g) <- uniformV' rng . V.fromList $
			[ (cs, pat, sz, g)
			| (cs, g) <- HM.toList (iGenes ind)
			, let sz = gSize g
			, pat <- [0..sz-1]
			]
		iInsert ind cs <$> f cs pat sz g

onGeneClone :: GenIO -> Vector Individual -> (ConvolutionSize -> Int -> Int -> Genome -> IO ()) -> IO Individual
onGeneClone rng pop f = onGene rng pop \cs pat sz g_ -> do
	g <- gClone g_
	g <$ f cs pat sz g

iGenome :: MsMendelConfig -> Individual -> ConvolutionSize -> IO Genome
iGenome mmc ind cs = case HM.lookup cs (iGenes ind) of
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
