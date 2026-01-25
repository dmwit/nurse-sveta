{-# Language AllowAmbiguousTypes #-}
{-# Language DataKinds #-}

module Ms.Mendel.Population
	( module Ms.Mendel.Population
	, WithSentinels(..)
	) where

import Control.Arrow
import Data.Aeson.Encoding (list, shortText, string)
import Data.ByteString (ByteString)
import Ms.Mendel.CXX.Cooked
import Nurse.Sveta.Util
import Nurse.Sveta.Widget

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

data Purpose = Authoring | Browsing | Disk | Gtk deriving (Bounded, Enum, Eq, Ord, Read, Show)
type Authoring = 'Authoring
type Browsing = 'Browsing
type Disk = 'Disk
type Gtk = 'Gtk

data family PatternGroups (a :: Purpose)
data family PatternGroup (a :: Purpose)
data family Pattern (a :: Purpose)
data family PatternTemplate (a :: Purpose)
data family Population (a :: Purpose)
data family Shared (a :: Purpose)
data family Individual (a :: Purpose)
data family SingleVirus (a :: Purpose)

class Repurpose dst src (t :: Purpose -> *) where
	type family RepurposingEnvironment dst src t
	repurpose :: RepurposingEnvironment dst src t -> t src -> t dst

	type instance RepurposingEnvironment dst src t = ()
	default repurpose :: src ~ dst => RepurposingEnvironment dst src t -> t src -> t dst
	repurpose _ = id

class RepurposeIO dst src (t :: Purpose -> *) where
	type family RepurposingEnvironmentIO dst src t
	repurposeIO :: RepurposingEnvironmentIO dst src t -> t src -> IO (t dst)

	type instance RepurposingEnvironmentIO dst src t = RepurposingEnvironment dst src t
	default repurposeIO :: (Repurpose dst src t, RepurposingEnvironment dst src t ~ RepurposingEnvironmentIO dst src t) => RepurposingEnvironmentIO dst src t -> t src -> IO (t dst)
	repurposeIO env = pure . repurpose env

type Repurpose' dst src t = (Repurpose dst src t, RepurposingEnvironment dst src t ~ ())
type RepurposeIO' dst src t = (RepurposeIO dst src t, RepurposingEnvironmentIO dst src t ~ ())

repurpose' :: forall dst src t. Repurpose' dst src t => t src -> t dst
repurpose' = repurpose ()

repurposeIO' :: forall dst src t. RepurposeIO' dst src t => t src -> IO (t dst)
repurposeIO' = repurposeIO ()

-- | for errors and the like
class HasFamilyName (f :: Purpose -> *) where familyName :: String
type LoadableDisk f = (FromJSON (f Disk), HasFamilyName f) :: Constraint
type Loadable    f a = (LoadableDisk f, RepurposeIO  a Disk f) :: Constraint
type Loadable'   f a = (LoadableDisk f, RepurposeIO' a Disk f) :: Constraint
type SavableDisk f = (ToJSON (f Disk), HasFamilyName f) :: Constraint
type Savable    f a = (SavableDisk f, RepurposeIO  Disk a f) :: Constraint
type Savable'   f a = (SavableDisk f, RepurposeIO' Disk a f) :: Constraint

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

newtype instance Individual Disk = IndividualDisk
	{ idParameters :: IndexedBy ParameterIndex R
	} deriving (Eq, Ord, Read, Show)

newtype instance PatternGroups Browsing = PatternGroupsBrowsing
	{ pgbPatternGroups :: IndexedBy PatternGroupIndex (PatternGroup Browsing)
	} deriving (Eq, Ord, Read, Show)

data instance PatternGroup Browsing = PatternGroupBrowsing
	{ pgbDescription :: Text
	, pgbPatterns :: IndexedBy IntraGroupIndex (Pattern Browsing)
	} deriving (Eq, Ord, Read, Show)

data instance Pattern Browsing = PatternBrowsing
	{ pbReplication :: Replication Bool
	, pbTemplate :: PatternTemplate Browsing
	} deriving (Eq, Ord, Read, Show)

newtype instance PatternTemplate Browsing = PatternTemplateBrowsing
	{ ptbCells :: IndexedBy Y0Lo (IndexedBy X PatternCell) -- ^ rectangular
	} deriving (Eq, Ord, Read, Show)

data instance Population Browsing = PopulationBrowsing
	{ pbGeneration :: Int
	, pbShared :: Shared Browsing
	, pbIndividuals :: IndexedBy IndividualIndex (Individual Browsing)
	} deriving (Eq, Ord, Read, Show)

data instance Shared Browsing = SharedBrowsing
	{ sbPatterns :: Map (Replication Bool) (Map ConvolutionSize (Set (PatternTemplate Browsing)))
	, sbStatisticNames :: IndexedBy StatisticParameter Text
	} deriving (Eq, Ord, Read, Show)

data instance Individual Browsing = IndividualBrowsing
	{ ib0, ib84 :: SingleVirus Browsing
	} deriving (Eq, Ord, Read, Show)

data instance SingleVirus Browsing = SingleVirusBrowsing
	{ svbPosition :: IndexedBy PatternParameter R
	, svbMove :: IndexedBy PatternParameter R
	, svbStatistics :: IndexedBy StatisticParameter R
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

instance HasFamilyName PatternGroups   where familyName = "pattern groups"
instance HasFamilyName PatternGroup    where familyName = "pattern group"
instance HasFamilyName Pattern         where familyName = "pattern"
instance HasFamilyName PatternTemplate where familyName = "pattern template"
instance HasFamilyName Population      where familyName = "population"
instance HasFamilyName Shared          where familyName = "shared"
instance HasFamilyName Individual      where familyName = "individual"
instance HasFamilyName SingleVirus     where familyName = "single virus"

---------- PatternGroups Authoring ----------

pgaByMetadata :: PatternGroups Authoring -> Map (Replication Bool) (Map ConvolutionSize (Set (PatternTemplate Browsing)))
pgaByMetadata = pgbByMetadata . repurpose'

caPatternGroupsName :: IsString s => s
caPatternGroupsName = "groups"

instance RepurposeIO Browsing Authoring PatternGroups
instance Repurpose Browsing Authoring PatternGroups where
	repurpose _ pga = PatternGroupsBrowsing
		{ pgbPatternGroups = repurpose (pgaDefaultReplication pga) <$> pgaPatternGroups pga
		}

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

instance RepurposeIO Authoring Authoring PatternGroups
instance Repurpose Authoring Authoring PatternGroups

---------- PatternGroup Authoring ----------

pgaPatternsName, pgaDescriptionName :: IsString s => s
pgaPatternsName = "patterns"
pgaDescriptionName = "description"

instance RepurposeIO Browsing Authoring PatternGroup
instance Repurpose Browsing Authoring PatternGroup where
	type instance RepurposingEnvironment Browsing Authoring PatternGroup = Replication Bool
	repurpose r pga = PatternGroupBrowsing
		{ pgbDescription = pgaDescription pga
		, pgbPatterns = repurpose (liftA2 fromMaybe r (pgaOverrideReplication pga)) <$> pgaPatterns pga
		}

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

instance RepurposeIO Authoring Authoring PatternGroup
instance Repurpose Authoring Authoring PatternGroup

---------- Pattern Authoring ----------

paTemplateName :: IsString s => s
paTemplateName = "pattern"

instance RepurposeIO Browsing Authoring Pattern
instance Repurpose Browsing Authoring Pattern where
	type instance RepurposingEnvironment Browsing Authoring Pattern = Replication Bool
	repurpose r pa = PatternBrowsing
		{ pbReplication = liftA2 fromMaybe r (paOverrideReplication pa)
		, pbTemplate = repurpose' (paTemplate pa)
		}

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

instance RepurposeIO Authoring Authoring Pattern
instance Repurpose Authoring Authoring Pattern

---------- PatternTemplate Authoring ----------

instance RepurposeIO Browsing Authoring PatternTemplate
instance Repurpose Browsing Authoring PatternTemplate where
	repurpose _ pta = PatternTemplateBrowsing
		{ ptbCells = toRectangle pcAnything . V.reverse $ ptaCells pta
		}

instance FromJSON (PatternTemplate Authoring) where
	parseJSON v = do
		cells <- parseJSON v
		when (all null cells) (fail "empty patterns are not supported")
		pure PatternTemplateAuthoring { ptaCells = cells }

instance RepurposeIO Authoring Authoring PatternTemplate
instance Repurpose Authoring Authoring PatternTemplate

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

instance RepurposeIO Browsing Disk Population
instance Repurpose Browsing Disk Population where
	repurpose _ pd = PopulationBrowsing
		{ pbGeneration = pdGeneration pd
		, pbShared = sb
		, pbIndividuals = repurpose sb <$> pdIndividuals pd
		} where sb = repurpose' (pdShared pd)

instance RepurposeIO Disk Disk Population
instance Repurpose Disk Disk Population

---------- Shared Disk ----------

sdPatternsName, sdStatisticNamesName :: IsString s => s
sdPatternsName = "patterns"
sdStatisticNamesName = "statistic-names"

-- could be optimized a bit by decoding the Texts and then just querying their size rather than reading in all the values in the patterns
-- (if you do that, maybe add a quickcheck test that it behaves the same as this spec)
sdParameterCount :: Shared Disk -> Int
sdParameterCount = sbParameterCount . repurpose'

instance RepurposeIO Browsing Disk Shared
instance Repurpose Browsing Disk Shared where
	-- safety: we briefly construct a fresh mutable value via ptFromText, but we immediately read and discard it via ptToSet
	repurpose _ sd = unsafePerformIO do
		patterns <- traverse (traverse (ptFromText >=> ptToSet)) (sdPatterns sd)
		let sizes = fmap (fmap (ptbConvolutionSize . S.findMin)) patterns
		unless (all strictlyAscending sizes) . fail $
			"malformed Shared Disk: PatternsTemplates were not in order of increasing size\n"
			++ show patterns
		pure SharedBrowsing
			{ sbStatisticNames = sdStatisticNames sd
			, sbPatterns = M.intersectionWith
				(\size pat -> M.fromList . V.toList $ V.zip size pat)
				sizes patterns
			}

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

instance RepurposeIO Disk Disk Shared
instance Repurpose Disk Disk Shared

---------- Individual Disk ----------

instance RepurposeIO Browsing Disk Individual
instance Repurpose Browsing Disk Individual where
	type instance RepurposingEnvironment Browsing Disk Individual = Shared Browsing
	repurpose sb id = IndividualBrowsing
		{ ib0 = newSingleVirusBrowsing sb ps0
		, ib84 = newSingleVirusBrowsing sb ps84
		} where
		n = length ps `quot` 2
		ps = idParameters id
		(ps0, ps84) = V.splitAt n ps

instance ToJSON (Individual Disk) where
	toEncoding = toEncoding . idParameters
	toJSON = toJSON . idParameters

instance FromJSON (Individual Disk) where
	parseJSON vs = IndividualDisk <$> parseJSON vs

instance RepurposeIO Disk Disk Individual
instance Repurpose Disk Disk Individual

---------- PatternGroups Browsing ----------

pgbByMetadata :: PatternGroups Browsing -> Map (Replication Bool) (Map ConvolutionSize (Set (PatternTemplate Browsing)))
pgbByMetadata pgsb = M.fromListWith (M.unionWith S.union)
	[ (pbReplication pb, M.singleton (pbConvolutionSize pb) (S.singleton (pbTemplate pb)))
	| pgb <- V.toList (pgbPatternGroups pgsb)
	, pb <- V.toList (pgbPatterns pgb)
	]

instance RepurposeIO Browsing Browsing PatternGroups
instance Repurpose Browsing Browsing PatternGroups

---------- PatternGroup Browsing ----------

instance Default (PatternGroup Browsing) where
	def = PatternGroupBrowsing mempty mempty

instance GNamed (PatternGroup Browsing) where name = "PatternGroup-Browsing"

instance RepurposeIO Browsing Browsing PatternGroup
instance Repurpose Browsing Browsing PatternGroup

---------- Pattern Browsing ----------

pbConvolutionSize :: Pattern Browsing -> ConvolutionSize
pbConvolutionSize = ptbConvolutionSize . pbTemplate

pbMetadata :: Pattern Browsing -> PatternMetadata
pbMetadata pc = PatternMetadata
	{ pmReplication = pbReplication pc
	, pmConvolutionSize = pbConvolutionSize pc
	}

instance Hashable (Pattern Browsing) where
	s `hashWithSalt` pc = s `hashWithSalt` pbReplication pc `hashWithSalt` pbTemplate pc

instance RepurposeIO Browsing Browsing Pattern
instance Repurpose Browsing Browsing Pattern

---------- PatternTemplate Browsing ----------

toRectangle :: a -> Vector (Vector a) -> Vector (Vector a)
toRectangle pad xss = (\row -> row <> V.replicate (w - length row) pad) <$> xss where
	w = V.maximum (V.singleton 0 <> V.map V.length xss)

ptbConvolutionSize :: PatternTemplate Browsing -> ConvolutionSize
ptbConvolutionSize ptb = ConvolutionSize
	{ csWidth = maybe 0 V.length $ ptbCells ptb V.!? 0
	, csHeight = V.length (ptbCells ptb)
	}

instance Hashable (PatternTemplate Browsing) where
	hashWithSalt s = hashWithSalt s . ptbCells

instance RepurposeIO Browsing Browsing PatternTemplate
instance Repurpose Browsing Browsing PatternTemplate

---------- Population Browsing ----------

newNormalPopulationBrowsing :: GenIO -> Shared Browsing -> Int -> IO (Population Browsing)
newNormalPopulationBrowsing rng sb populationSize = do
	is <- V.replicateM populationSize (newNormalIndividualBrowsing rng sb)
	pure PopulationBrowsing
		{ pbGeneration = 0
		, pbShared = sb
		, pbIndividuals = is
		}

instance RepurposeIO Disk Browsing Population
instance Repurpose Disk Browsing Population where
	repurpose _ pb = PopulationDisk
		{ pdGeneration = pbGeneration pb
		, pdShared = repurpose' (pbShared pb)
		, pdIndividuals = fmap repurpose' (pbIndividuals pb)
		}

instance RepurposeIO Browsing Browsing Population
instance Repurpose Browsing Browsing Population

---------- Shared Browsing ----------

sbPatternCount :: Shared Browsing -> Int
sbPatternCount = sum . fmap (sum . fmap S.size) . sbPatterns

sbStatisticCount :: Shared Browsing -> Int
sbStatisticCount = length . sbStatisticNames

sbParameterCount :: Shared Browsing -> Int
sbParameterCount sb = 2{- 0 virus/84 virus -} * (2{- position/move -} * sbPatternCount sb + sbStatisticCount sb)

sbFromPatternGroups :: PatternGroups Browsing -> Shared Browsing
sbFromPatternGroups pgb = SharedBrowsing
	{ sbPatterns = pgbByMetadata pgb
	, sbStatisticNames = currentStatisticNames
	}

instance RepurposeIO Disk Browsing Shared
instance Repurpose Disk Browsing Shared where
	repurpose _ sb = SharedDisk
		-- safety: we briefly create a mutable thing with ptsFromMap, but we immediately do a complete read of it into a pure value with ptsToText and throw it away
		{ sdPatterns = unsafePerformIO $ traverse (ptsFromMap >=> ptsToText) (sbPatterns sb)
		, sdStatisticNames = sbStatisticNames sb
		}

instance RepurposeIO Browsing Browsing Shared
instance Repurpose Browsing Browsing Shared

---------- Individual Browsing ----------

newNormalIndividualBrowsing :: GenIO -> Shared Browsing -> IO (Individual Browsing)
newNormalIndividualBrowsing rng sb = id
	. repurpose sb
	. IndividualDisk
	. fmap realToFrac
	<$> V.replicateM (sbParameterCount sb) (standard rng)

instance RepurposeIO Disk Browsing Individual
instance Repurpose Disk Browsing Individual where
	repurpose _ ib = IndividualDisk $ mempty
		<> svbParameters (ib0 ib)
		<> svbParameters (ib84 ib)

instance RepurposeIO Browsing Browsing Individual
instance Repurpose Browsing Browsing Individual

---------- SingleVirus Browsing ----------

newSingleVirusBrowsing :: HasCallStack => Shared Browsing -> IndexedBy ParameterIndex R -> SingleVirus Browsing
newSingleVirusBrowsing sb ps
	| length ps == n = SingleVirusBrowsing
		{ svbPosition = V.take nPat ps
		, svbMove = V.take nPat (V.drop nPat ps)
		, svbStatistics = V.drop (2*nPat) ps
		}
	| otherwise = error $ printf "couldn't parse vector as a SingleVirus Browsing; expected length %d but saw length %d" n (length ps)
	where
	n = 2*nPat + nStat
	nPat = sbPatternCount sb
	nStat = sbStatisticCount sb

svbParameters :: SingleVirus Browsing -> IndexedBy ParameterIndex R
svbParameters svb = svbPosition svb <> svbMove svb <> svbStatistics svb

instance RepurposeIO Browsing Browsing SingleVirus
instance Repurpose Browsing Browsing SingleVirus

---------- PatternCell ----------

newPatternCell :: Set (WithSentinels (Either Color Shape)) -> PatternCell
newPatternCell = PatternCell . S.map \case
	NonSentinel (Right North) -> NonSentinel (Right Disconnected)
	NonSentinel (Right South) -> NonSentinel (Right Disconnected)
	other -> other

pcNormalize :: PatternCell -> PatternCell
pcNormalize = newPatternCell . pcAllowed

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

pcNothing :: PatternCell
pcNothing = PatternCell { pcAllowed = S.empty }

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
pmToTuple = pmConvolutionSize &&& pmReplication

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
csToTuple = csWidth &&& csHeight

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
rToTuple = rMirroring &&& rColoring

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

currentStatisticNames :: IndexedBy StatisticParameter Text
currentStatisticNames = V.empty

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

strictlyAscending :: Ord a => Vector a -> Bool
strictlyAscending as = and (V.zipWith (<) as (V.drop 1 as))

ptFromText :: Text -> IO PatternsTemplate
ptFromText = ptDecode . decodePrintable

ptFromSet :: ConvolutionSize -> Set (PatternTemplate Browsing) -> IO PatternsTemplate
ptFromSet cs cells = do
	pt <- newPatternsTemplate (fromIntegral (csWidth cs)) (fromIntegral (csHeight cs)) (fromIntegral (S.size cells))
	pt <$ forZipWithM_ [0..] (S.toList cells) \i ptb ->
		forZipWithM_ [0..] (V.toList (ptbCells ptb)) \y row ->
			forZipWithM_ [0..] (V.toList row) \x cell ->
				for_ (S.toList (pcAllowed cell)) \case
					NonSentinel (Left color) -> ptSetColor pt i (NonSentinel color) (Position x y) False
					NonSentinel (Right shape) -> ptSetShape pt i (NonSentinel shape) (Position x y) False
					OutOfBoundsSentinel -> ptSetBoth pt i OutOfBoundsSentinel (Position x y) False
					EmptySentinel -> ptSetBoth pt i EmptySentinel (Position x y) False

ptsFromMap :: Map ConvolutionSize (Set (PatternTemplate Browsing)) -> IO (IndexedBy ConvolutionSizeIndex PatternsTemplate)
ptsFromMap = traverse (uncurry ptFromSet) . V.fromList . M.toAscList

ptToText :: PatternsTemplate -> IO Text
ptToText = fmap encodePrintable . ptEncode

ptsToText :: IndexedBy ConvolutionSizeIndex PatternsTemplate -> IO (IndexedBy ConvolutionSizeIndex Text)
ptsToText = traverse ptToText

ptToVector :: PatternsTemplate -> IO (Vector (PatternTemplate Browsing))
ptToVector pt = V.generateM (fromIntegral (ptSize pt)) \i_ -> do
	let i = fromIntegral i_
	PatternTemplateBrowsing <$> V.generateM (fromIntegral (ptHeight pt)) \y ->
		V.generateM (fromIntegral (ptWidth pt)) \x -> let pos = Position x y in
			PatternCell . S.fromList <$> flip filterM pcAllDisjuncts \disjunct -> not <$> case disjunct of
				NonSentinel (Left color) -> ptGetColor pt i (NonSentinel color) pos
				NonSentinel (Right shape) -> ptGetShape pt i (NonSentinel shape) pos
				OutOfBoundsSentinel -> ptGetBoth pt i OutOfBoundsSentinel pos
				EmptySentinel -> ptGetBoth pt i EmptySentinel pos

ptToSet :: PatternsTemplate -> IO (Set (PatternTemplate Browsing))
ptToSet pt = do
	v <- ptToVector pt
	unless (strictlyAscending v) (fail $ "malformed PatternsTemplate had templates in wrong order: " ++ show v)
	pure . S.fromAscList . V.toList $ v

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
