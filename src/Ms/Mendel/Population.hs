{-# Language DataKinds #-}
module Ms.Mendel.Population where

import Data.Aeson.Encoding (list, shortText, string)
import Data.ByteString (ByteString)
import Ms.Mendel.CXX.Cooked
import Nurse.Sveta.Util

import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

type IndexedBy a = Vector

type PatternGroupIndex = Int -- in the order that pattern groups appear in the configuration file
type X = Int
type Y0Lo = Int
type Y0Hi = Int
type IntraGroupIndex = Int -- in the order that patterns appear in this group in the configuration file
type ParameterIndex = Int
type PatternParameter = Int
type StatisticParameter = Int
type ConvolutionSizeIndex = Int
type VirusCount = Int
type IndividualIndex = Int

type R = Float
type Tanh = Float
type Rn = Tensor
type Tanhn = Tensor

data Purpose = Authoring | Browsing | Disk deriving (Bounded, Enum, Eq, Ord, Read, Show)
type Authoring = 'Authoring
type Browsing = 'Browsing
type Disk = 'Disk

data family PatternGroups (a :: Purpose)
data family PatternGroup (a :: Purpose)
data family Pattern (a :: Purpose)
data family PatternTemplate (a :: Purpose)
data family Population (a :: Purpose)
data family Shared (a :: Purpose)
data family Individual (a :: Purpose)
data family SingleVirus (a :: Purpose)
data family SplitScores (a :: Purpose)

class Repurpose (t :: Purpose -> *) src dst where
	type family RepurposingEnvironment t src dst
	repurpose :: RepurposingEnvironment t src dst -> t src -> t dst

repurpose_ :: (Repurpose t src dst, RepurposingEnvironment t src dst ~ ()) => t src -> t dst
repurpose_ = repurpose ()

data instance PatternGroups Authoring = PatternGroupsAuthoring
	{ pgaDefaultReplication :: Replication Bool
	, pgaPatternGroups :: IndexedBy PatternGroupIndex (PatternGroup Authoring)
	} deriving (Eq, Ord, Read, Show)

data instance PatternGroup Authoring = PatternGroupAuthoring
	{ pgaOverrideReplication :: Replication (Maybe Bool)
	, pgaPatterns :: IndexedBy IntraGroupIndex (Pattern Authoring)
	, pgaDescription :: Text
	} deriving (Eq, Ord, Read, Show)

data instance Pattern Authoring = PatternAuthoring
	{ paOverrideReplication :: Replication (Maybe Bool)
	, paTemplate :: PatternTemplate Authoring
	} deriving (Eq, Ord, Read, Show)

newtype instance PatternTemplate Authoring = PatternTemplateAuthoring
	{ ptaCells :: IndexedBy Y0Hi (IndexedBy X PatternCell) -- ^ not necessarily rectangular
	} deriving (Eq, Ord, Read, Show)

data instance Population Disk = PopulationDisk
	{ pdGeneration :: Int
	, pdShared :: Shared Disk
	, pdIndividuals :: IndexedBy IndividualIndex (Individual Disk)
	} deriving (Eq, Ord, Read, Show)

data instance Shared Disk = SharedDisk
	{ sdPatterns :: Map (Replication Bool) (IndexedBy ConvolutionSizeIndex Text) -- ^ the 'Text' is secretly a 'ByteString'
	, sdStatisticNames :: IndexedBy StatisticParameter Text
	} deriving (Eq, Ord, Read, Show)

data instance Individual Disk = IndividualDisk
	{ id0, id84 :: SingleVirus Disk
	} deriving (Eq, Ord, Read, Show)

data instance SingleVirus Disk = SingleVirusDisk
	{ svdPosition :: SplitScores Disk
	, svdMove :: SplitScores Disk
	} deriving (Eq, Ord, Read, Show)

data instance SplitScores Disk = SplitScoresDisk
	{ ssdPatternScores :: IndexedBy PatternParameter R
	, ssdStatisticScores :: IndexedBy StatisticParameter R
	} deriving (Eq, Ord, Read, Show)

data instance PatternGroup Browsing = PatternGroupBrowsing
	{ pgbPatterns :: IndexedBy IntraGroupIndex (Pattern Browsing)
	} deriving (Show)

data instance Pattern Browsing = PatternBrowsing
	{ pbReplication :: Replication Bool
	, pbTemplate :: PatternTemplate Browsing
	} deriving (Eq, Ord, Read, Show)

newtype instance PatternTemplate Browsing = PatternTemplateBrowsing
	{ ptbCells :: IndexedBy Y0Lo (IndexedBy X PatternCell) -- ^ rectangular
	} deriving (Eq, Ord, Read, Show)

newtype PatternCell = PatternCell
	{ pcAllowed :: Set (WithSentinels (Either Color Shape))
	} deriving (Eq, Ord, Read, Show)

data PatternMetadata = PatternMetadata
	{ pmConvolutionSize :: ConvolutionSize
	, pmReplication :: Replication Bool
	} deriving (Eq, Ord, Read, Show)

data ConvolutionSize = ConvolutionSize
	{ csWidth, csHeight :: Int
	} deriving (Eq, Ord, Read, Show)

data Replication a = Replication
	{ rMirroring :: a
	, rColoring :: a
	} deriving (Eq, Ord, Read, Show, Functor)

---------- PatternGroups Authoring ----------

caPatternGroupsName :: IsString s => s
caPatternGroupsName = "groups"

instance FromJSON (PatternGroups Authoring) where
	parseJSON (Object o) = do
		assertOnly [rMirroringName, rColoringName, caPatternGroupsName] o
		replication <- rFromKeyMap o
		groups <- o .: caPatternGroupsName
		pure PatternGroupsAuthoring
			{ pgaDefaultReplication = replication
			, pgaPatternGroups = groups
			}
	parseJSON other = typeMismatch
		("PatternGroups (an object with \"" ++ caPatternGroupsName ++ "\", \"" ++ rMirroringName ++ "\", and \"" ++ rColoringName ++ "\" keys)")
		other

---------- PatternGroup Authoring ----------

pgaPatternsName, pgaDescriptionName :: IsString s => s
pgaPatternsName = "patterns"
pgaDescriptionName = "description"

instance FromJSON (PatternGroup Authoring) where
	parseJSON (Object o) = do
		assertOnly [rMirroringName, rColoringName, pgaPatternsName, pgaDescriptionName] o
		replication <- rMaybeFromKeyMap o
		patterns <- o .: pgaPatternsName
		description <- o .:? pgaDescriptionName
		pure PatternGroupAuthoring
			{ pgaOverrideReplication = replication
			, pgaPatterns = patterns
			, pgaDescription = fromMaybe "" description
			}
	parseJSON a@(Array _) = do
		patterns <- parseJSON a
		pure PatternGroupAuthoring
			{ pgaOverrideReplication = pure Nothing
			, pgaPatterns = patterns
			, pgaDescription = ""
			}
	parseJSON other = typeMismatch
		("PatternGroup (a list of Patterns, or an object with a \"" ++ pgaPatternsName ++ "\" key and optionally \"" ++ pgaDescriptionName ++ "\", \"" ++ rMirroringName ++ "\", and \"" ++ rColoringName ++ "\" keys)")
		other

---------- Pattern Authoring ----------

paTemplateName :: IsString s => s
paTemplateName = "pattern"

instance FromJSON (Pattern Authoring) where
	parseJSON (Object o) = do
		assertOnly [rMirroringName, rColoringName, paTemplateName] o
		replication <- rFromKeyMap o
		template <- o .: paTemplateName
		pure PatternAuthoring
			{ paOverrideReplication = replication
			, paTemplate = template
			}
	parseJSON a@(Array _) = do
		template <- parseJSON a
		pure PatternAuthoring
			{ paOverrideReplication = pure Nothing
			, paTemplate = template
			}
	parseJSON other = typeMismatch
		("Pattern (a list of list of PatternCells, or an object with a \"" ++ paTemplateName ++ "\" key and optionally \"" ++ rMirroringName ++ "\" and \"" ++ rColoringName ++ "\" keys)")
		other

---------- PatternTemplate Authoring ----------

instance FromJSON (PatternTemplate Authoring) where
	parseJSON v = do
		cells <- parseJSON v
		when (all null cells) (fail "empty patterns are not supported")
		pure PatternTemplateAuthoring { ptaCells = cells }

---------- Population Disk ----------

pdGenerationName, pdSharedName, pdIndividualsName :: IsString s => s
pdGenerationName = "generation"
pdSharedName = "shared"
pdIndividualsName = "individuals"

instance ToJSON (Population Disk) where
	toEncoding pd = pairs $ mempty
		<> pdGenerationName .= pdGeneration pd
		<> pdSharedName .= pdShared pd
		<> pdIndividualsName .= pdIndividuals pd
	toJSON pd = object $ tail [ignored
		, pdGenerationName .= pdGeneration pd
		, pdSharedName .= pdShared pd
		, pdIndividualsName .= pdIndividuals pd
		]

instance FromJSON (Population Disk) where
	parseJSON (Object o) = do
		generation <- o .: pdGenerationName
		shared <- o .: pdSharedName
		individuals <- o .: pdIndividualsName
		assertOnly [pdGenerationName, pdSharedName, pdIndividualsName] o
		pure PopulationDisk
			{ pdGeneration = generation
			, pdShared = shared
			, pdIndividuals = individuals
			}

---------- Shared Disk ----------

sdPatternCount :: Shared Disk -> Int
sdPatternCount sd = sum (length <$> sdPatterns sd)

sdStatisticCount :: Shared Disk -> Int
sdStatisticCount = length . sdStatisticNames

sdPatternsName, sdStatisticNamesName :: IsString s => s
sdPatternsName = "patterns"
sdStatisticNamesName = "statistic-names"

instance ToJSON (Shared Disk) where
	toEncoding sd = pairs $ mempty
		<> sdPatternsName .= sdPatterns sd
		<> sdStatisticNamesName .= sdStatisticNames sd
	toJSON sd = object $ tail [ignored
		, sdPatternsName .= sdPatterns sd
		, sdStatisticNamesName .= sdStatisticNames sd
		]

instance FromJSON (Shared Disk) where
	parseJSON (Object o) = do
		patterns <- o .: sdPatternsName
		statisticNames <- o .: sdStatisticNamesName
		assertOnly [sdPatternsName, sdStatisticNamesName] o
		pure SharedDisk
			{ sdPatterns = patterns
			, sdStatisticNames = statisticNames
			}

---------- Individual Disk ----------

newIndividualDisk :: Shared Disk -> IndexedBy ParameterIndex R -> Individual Disk
newIndividualDisk sd ps = IndividualDisk
	{ id0 = newSingleVirusDisk sd (V.take n ps)
	, id84 = newSingleVirusDisk sd (V.drop n ps)
	} where n = length ps `quot` 2

idForget :: Individual Disk -> IndexedBy ParameterIndex R
idForget id = svdForget (id0 id) <> svdForget (id84 id)

idToTuple :: Individual Disk -> (SingleVirus Disk, SingleVirus Disk)
idToTuple id = (id0 id, id84 id)

idFromTuple :: (SingleVirus Disk, SingleVirus Disk) -> Individual Disk
idFromTuple (svd0, svd84) = IndividualDisk { id0 = svd0 , id84 = svd84 }

instance ToJSON (Individual Disk) where
	toEncoding = toEncoding . idToTuple
	toJSON = toJSON . idToTuple

instance FromJSON (Individual Disk) where
	parseJSON vs = idFromTuple <$> parseJSON vs

---------- SingleVirus Disk ----------

newSingleVirusDisk :: Shared Disk -> IndexedBy ParameterIndex R -> SingleVirus Disk
newSingleVirusDisk sd ps = SingleVirusDisk
	{ svdPosition = newSplitScoresDisk sd (V.take n ps)
	, svdMove = newSplitScoresDisk sd (V.drop n ps)
	} where n = length ps `quot` 2

svdForget :: SingleVirus Disk -> IndexedBy ParameterIndex R
svdForget svd = ssdForget (svdPosition svd) <> ssdForget (svdMove svd)

svdToPair :: SingleVirus Disk -> (SplitScores Disk, SplitScores Disk)
svdToPair svd = (svdPosition svd, svdMove svd)

svdFromPair :: (SplitScores Disk, SplitScores Disk) -> SingleVirus Disk
svdFromPair (sdPos, sdMove) = SingleVirusDisk { svdPosition = sdPos, svdMove = sdMove }

instance ToJSON (SingleVirus Disk) where
	toEncoding = toEncoding . svdToPair
	toJSON = toJSON . svdToPair

instance FromJSON (SingleVirus Disk) where
	parseJSON vs = svdFromPair <$> parseJSON vs

---------- SplitScores Disk ----------

newSplitScoresDisk :: Shared Disk -> IndexedBy ParameterIndex R -> SplitScores Disk
newSplitScoresDisk sd ps
	| length ps == n = SplitScoresDisk
		{ ssdPatternScores = V.take nPatterns ps
		, ssdStatisticScores = V.drop nPatterns ps
		}
	| otherwise = error $ printf "couldn't parse vector as a SplitScores Disk; expected length %d but saw length %d" n (length ps)
	where
	n = nPatterns + nStatistics
	nPatterns = sdPatternCount sd
	nStatistics = sdStatisticCount sd

ssdForget :: SplitScores Disk -> IndexedBy ParameterIndex R
ssdForget sd = ssdPatternScores sd <> ssdStatisticScores sd

ssdToTuple :: SplitScores Disk -> (IndexedBy PatternParameter R, IndexedBy StatisticParameter R)
ssdToTuple sd = (ssdPatternScores sd, ssdStatisticScores sd)

ssdFromTuple :: (IndexedBy PatternParameter R, IndexedBy StatisticParameter R) -> SplitScores Disk
ssdFromTuple (vPat, vStat) = SplitScoresDisk { ssdPatternScores = vPat, ssdStatisticScores = vStat }

instance ToJSON (SplitScores Disk) where
	toEncoding = toEncoding . ssdToTuple
	toJSON = toJSON . ssdToTuple

instance FromJSON (SplitScores Disk) where
	parseJSON vs = ssdFromTuple <$> parseJSON vs

---------- PatternGroup Browsing ----------

instance Repurpose PatternGroup Authoring Browsing where
	type instance RepurposingEnvironment PatternGroup Authoring Browsing = Replication Bool
	repurpose r pga = PatternGroupBrowsing
		{ pgbPatterns = repurpose (liftA2 fromMaybe r (pgaOverrideReplication pga)) <$> pgaPatterns pga
		}

---------- Pattern Browsing ----------

instance Repurpose Pattern Authoring Browsing where
	type instance RepurposingEnvironment Pattern Authoring Browsing = Replication Bool
	repurpose r pa = PatternBrowsing
		{ pbReplication = liftA2 fromMaybe r (paOverrideReplication pa)
		, pbTemplate = repurpose_ (paTemplate pa)
		}

pbConvolutionSize :: Pattern Browsing -> ConvolutionSize
pbConvolutionSize = ptbConvolutionSize . pbTemplate

pbMetadata :: Pattern Browsing -> PatternMetadata
pbMetadata pc = PatternMetadata
	{ pmReplication = pbReplication pc
	, pmConvolutionSize = pbConvolutionSize pc
	}

instance Hashable (Pattern Browsing) where
	s `hashWithSalt` pc = s `hashWithSalt` pbReplication pc `hashWithSalt` pbTemplate pc

---------- PatternTemplate Browsing ----------

toRectangle :: a -> Vector (Vector a) -> Vector (Vector a)
toRectangle pad xss = (\row -> row <> V.replicate (w - length row) pad) <$> xss where
	w = V.maximum (V.singleton 0 <> V.map V.length xss)

ptbConvolutionSize :: PatternTemplate Browsing -> ConvolutionSize
ptbConvolutionSize ptc = ConvolutionSize
	{ csWidth = V.head (V.map V.length (ptbCells ptc) <> V.singleton 0)
	, csHeight = V.length (ptbCells ptc)
	}

instance Repurpose PatternTemplate Authoring Browsing where
	type instance RepurposingEnvironment PatternTemplate Authoring Browsing = ()
	repurpose _ pta = PatternTemplateBrowsing
		{ ptbCells = toRectangle pcAnything . V.reverse $ ptaCells pta
		}

instance ToJSON (PatternTemplate Browsing) where
	toEncoding = toEncoding . V.reverse . ptbCells
	toJSON = toJSON . V.reverse . ptbCells

instance Hashable (PatternTemplate Browsing) where
	hashWithSalt s = hashWithSalt s . ptbCells

---------- PatternCell ----------

instance FromJSON PatternCell where
	parseJSON json = do
		t <- parseJSON json
		PatternCell <$> T.foldl' (\s c -> do
			disjunct <- case c of
				'b' -> pure $ NonSentinel (Left Blue)
				'r' -> pure $ NonSentinel (Left Red)
				'y' -> pure $ NonSentinel (Left Yellow)
				'x' -> pure $ NonSentinel (Right Virus)
				'<' -> pure $ NonSentinel (Right West)
				'>' -> pure $ NonSentinel (Right East)
				'o' -> pure $ NonSentinel (Right Disconnected)
				'|' -> pure $ OutOfBoundsSentinel
				'e' -> pure $ EmptySentinel
				'*' -> pure $ EmptySentinel -- we'll fix this up later
				_ -> fail $ "expected one of b, r, y, x, <, >, o, |, e, or *, but got " ++ [c]
			if c == '*' then pure (pcAllowed pcAnything) else S.insert disjunct <$> s
			) (pure S.empty) t

instance ToJSON PatternCell where
	toEncoding pc
		| pc == pcAnything = shortText "*"
		| otherwise = string . map disjunctToChar . S.toAscList . pcAllowed $ pc
	toJSON pc
		| pc == pcAnything = toJSON ['*']
		| otherwise = toJSON . map disjunctToChar . S.toAscList . pcAllowed $ pc

pcAllDisjuncts :: [WithSentinels (Either Color Shape)]
pcAllDisjuncts = []
	++ [NonSentinel (Left c) | c <- [minBound..maxBound]]
	++ [NonSentinel (Right s) | s <- [Virus, West, East, Disconnected]]
	++ [OutOfBoundsSentinel, EmptySentinel]

pcAnything :: PatternCell
pcAnything = PatternCell { pcAllowed = S.fromList pcAllDisjuncts }

disjunctToChar :: WithSentinels (Either Color Shape) -> Char
disjunctToChar = \case
	NonSentinel (Left Blue) -> 'b'
	NonSentinel (Left Red) -> 'r'
	NonSentinel (Left Yellow) -> 'y'
	NonSentinel (Right Virus) -> 'x'
	NonSentinel (Right Disconnected) -> 'o'
	NonSentinel (Right North) -> 'o'
	NonSentinel (Right South) -> 'o'
	NonSentinel (Right East) -> '>'
	NonSentinel (Right West) -> '<'
	OutOfBoundsSentinel -> '|'
	EmptySentinel -> 'e'

instance Hashable PatternCell where
	hashWithSalt s = hashWithSalt s . pcAllowed

---------- PatternMetadata ----------

pmToTuple :: PatternMetadata -> (ConvolutionSize, Replication Bool)
pmToTuple pm = (pmConvolutionSize pm, pmReplication pm)

pmFromTuple :: (ConvolutionSize, Replication Bool) -> PatternMetadata
pmFromTuple (cs, r) = PatternMetadata
	{ pmConvolutionSize = cs
	, pmReplication = r
	}

pmPretty :: PatternMetadata -> String
pmPretty pm = csPretty (pmConvolutionSize pm) ++ " " ++ rPretty (pmReplication pm)

pmFromText :: Text -> Parser PatternMetadata
pmFromText t = case T.words t of
	[cs, r] -> liftA2 PatternMetadata (csFromText cs) (rFromText r)
	_ -> typeMismatch "PatternMetadata (a string of the form \"cs r\" where cs is a ConvolutionSize and r is a Replication" (toJSON t)

instance ToJSON PatternMetadata where
	toEncoding = toEncoding . pmToTuple
	toJSON = toJSON . pmToTuple

instance FromJSON PatternMetadata where
	parseJSON v = pmFromTuple <$> parseJSON v

instance ToJSONKey PatternMetadata where
	toJSONKey = ToJSONKeyText (fromString . pmPretty) (fromString . pmPretty)

instance FromJSONKey PatternMetadata where
	fromJSONKey = FromJSONKeyTextParser pmFromText

instance Hashable PatternMetadata where
	s `hashWithSalt` pm = s `hashWithSalt` pmConvolutionSize pm `hashWithSalt` pmReplication pm

---------- ConvolutionSize ----------

csToTuple :: ConvolutionSize -> (Int, Int)
csToTuple cs = (csWidth cs, csHeight cs)

csFromTuple :: (Int, Int) -> ConvolutionSize
csFromTuple (w, h) = ConvolutionSize { csWidth = w, csHeight = h }

csPretty :: ConvolutionSize -> String
csPretty cs = show (csWidth cs) ++ "x" ++ show (csHeight cs)

csFromText :: Text -> Parser ConvolutionSize
csFromText t = case T.breakOnAll "x" t of
	[(treadMaybe -> Just w, treadMaybe . T.drop 1 -> Just h)] -> pure $ ConvolutionSize w h
	_ -> typeMismatch "ConvolutionSize (a string of the form \"wxh\" where w and h are ints)" (toJSON t)
	where treadMaybe = readMaybe . T.unpack

instance ToJSON ConvolutionSize where
	toEncoding = toEncoding . csToTuple
	toJSON = toJSON . csToTuple

instance FromJSON ConvolutionSize where
	parseJSON v = csFromTuple <$> parseJSON v

instance ToJSONKey ConvolutionSize where
	toJSONKey = ToJSONKeyText (fromString . csPretty) (fromString . csPretty)

instance FromJSONKey ConvolutionSize where
	fromJSONKey = FromJSONKeyTextParser csFromText

instance Hashable ConvolutionSize where
	s `hashWithSalt` cs = s `hashWithSalt` csWidth cs `hashWithSalt` csHeight cs

---------- Replication ----------

rToTuple :: Replication a -> (a, a)
rToTuple r = (rMirroring r, rColoring r)

rFromTuple :: (a, a) -> Replication a
rFromTuple (m, c) = Replication { rMirroring = m, rColoring = c }

rPretty :: Replication Bool -> String
-- plausible alternatives to π that my terminal unfortunately doesn't render well: 🌈 or the flags of Armenia, Chad, or Romania
rPretty r = [if rMirroring r then '↔' else '=', if rColoring r then 'π' else '=']

rFromText :: Text -> Parser (Replication Bool)
rFromText = \case
	"==" -> pure Replication { rMirroring = False, rColoring = False }
	"=π" -> pure Replication { rMirroring = False, rColoring = True  }
	"↔=" -> pure Replication { rMirroring = True , rColoring = False }
	"↔π" -> pure Replication { rMirroring = True , rColoring = True  }
	t -> typeMismatch "Replication (a string consisting of = or ↔ followed by = or π)" (toJSON t)

rFromKeyMap :: FromJSON a => KeyMap Value -> Parser (Replication a)
rFromKeyMap km = do
	mirroring <- km .: rMirroringName
	coloring <- km .: rColoringName
	pure Replication
		{ rMirroring = mirroring
		, rColoring = coloring
		}

rMaybeFromKeyMap :: FromJSON a => KeyMap Value -> Parser (Replication (Maybe a))
rMaybeFromKeyMap km = do
	mirroring <- km .:? rMirroringName
	coloring <- km .:? rColoringName
	pure Replication
		{ rMirroring = mirroring
		, rColoring = coloring
		}

rMirroringName, rColoringName :: IsString s => s
rMirroringName = "mirroring"
rColoringName = "coloring"

instance ToJSON a => ToJSON (Replication a) where
	toEncoding = toEncoding . rToTuple
	toJSON = toJSON . rToTuple

instance FromJSON a => FromJSON (Replication a) where
	parseJSON v = rFromTuple <$> parseJSON v

instance a ~ Bool => ToJSONKey (Replication a) where
	toJSONKey = ToJSONKeyText (fromString . rPretty) (fromString . rPretty)

instance a ~ Bool => FromJSONKey (Replication a) where
	fromJSONKey = FromJSONKeyTextParser rFromText

instance Hashable a => Hashable (Replication a) where
	s `hashWithSalt` r = s `hashWithSalt` rMirroring r `hashWithSalt` rColoring r

instance Applicative Replication where
	pure a = Replication a a
	Replication f f' <*> Replication x x' = Replication (f x) (f' x')

instance Monad Replication where
	return = pure
	m >>= f = Replication x x' where
		Replication (Replication x _) (Replication _ x') = f <$> m

---------- other ----------

assertOnly :: [Key] -> KeyMap v -> Parser ()
assertOnly ks km
	| null leftovers = pure ()
	| otherwise = fail $ "expected dictionary with only the keys " ++ pretty ks ++ "; extra keys were " ++ pretty (KM.keys leftovers)
	where
	leftovers = km `KM.difference` KM.fromList [(k, ()) | k <- ks]
	pretty = printf "{%s}" . intercalate ", " . map show

-- encodePrintable and decodePrintable convert between unconstrained byte
-- sequences and sequences of bytes that JSON can represent in one byte each:
-- " !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~"
-- this gets us about logBase 93 256 = 1.2234 bytes/byte on average
--
-- Trailing '\0's are dropped!
encodePrintable :: ByteString -> Text
encodePrintable = T.pack . expand . contract where
	contract = BS.foldr (\w ws -> toInteger w .|. shiftL ws 8) 0

	expand 0 = []
	expand n = toEnum byte : expand q where
		(q, r) = n `quotRem` 93
		byte = 32 + fromInteger r
			+ (if r <  2 then 0 else 1)
			+ (if r < 59 then 0 else 1)

decodePrintable :: Text -> ByteString
decodePrintable = BS.pack . expand . contract where
	contract = T.foldr (\w ws -> toInteger (byte (fromEnum w)) + 93 * ws) 0
	byte w = w - 32 - (if w < 34 then 0 else 1) - (if w < 93 then 0 else 1)

	expand 0 = []
	expand n = fromInteger n : expand (shiftR n 8)

ptFromSet :: ConvolutionSize -> Set [[PatternCell]] -> IO PatternsTemplate
ptFromSet cs cells = do
	pt <- newPatternsTemplate (fromIntegral (csWidth cs)) (fromIntegral (csHeight cs)) (fromIntegral (S.size cells))
	pt <$ forZipWithM_ [0..] (S.toList cells) \i rectangle ->
		forZipWithM_ [0..] rectangle \y row ->
			forZipWithM_ [0..] row \x cell ->
				for_ (S.toList (pcAllowed cell)) \case
					NonSentinel (Left color) -> ptSetColor pt i (NonSentinel color) (Position x y) False
					NonSentinel (Right shape) -> ptSetShape pt i (NonSentinel shape) (Position x y) False
					OutOfBoundsSentinel -> ptSetBoth pt i OutOfBoundsSentinel (Position x y) False
					EmptySentinel -> ptSetBoth pt i EmptySentinel (Position x y) False

ptToVector :: PatternsTemplate -> IO (Vector [[PatternCell]])
ptToVector pt = V.generateM (fromIntegral (ptSize pt)) \i_ -> do
	let i = fromIntegral i_
	for [0..ptHeight pt] \y ->
		for [0..ptWidth pt] \x -> let pos = Position (fromIntegral x) (fromIntegral y) in
			PatternCell . S.fromList <$> flip filterM pcAllDisjuncts \disjunct -> not <$> case disjunct of
				NonSentinel (Left color) -> ptGetColor pt i (NonSentinel color) pos
				NonSentinel (Right shape) -> ptGetShape pt i (NonSentinel shape) pos
				OutOfBoundsSentinel -> ptGetBoth pt i OutOfBoundsSentinel pos
				EmptySentinel -> ptGetBoth pt i EmptySentinel pos

ptSetBoth :: PatternsTemplate -> Int64 -> (forall a. WithSentinels a) -> Position -> Bool -> IO ()
ptSetBoth pt i ws pos v = do
	ptSetColor pt i ws pos v
	ptSetShape pt i ws pos v

ptGetBoth :: PatternsTemplate -> Int64 -> (forall a. WithSentinels a) -> Position -> IO Bool
ptGetBoth pt i ws pos = liftA2 (||)
	(ptGetColor pt i ws pos)
	(ptGetShape pt i ws pos)

class Lerp a where
	-- | @lerp f a0 a1@ forms the line containing the points @(0, a0)@ and @(1,
	-- a1)@, finds a point @(f, af)@ on the line, and returns @af@.
	lerp :: Float -> a -> a -> a

instance Lerp Tensor where lerp fraction t0 t1 = tScale fraction t1 `tAdd` tScale (1 - fraction) t0
instance Lerp Float  where lerp fraction f0 f1 = fraction * f1 + (1 - fraction) * f0
