{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}

import Data.Aeson.Encoding
import Data.Aeson.Key
import Data.ByteString (ByteString)
import Ms.Mendel
import Nurse.Sveta.GameBrowser (GameStateEdit(..))
import Test.QuickCheck

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Nurse.Sveta.GameBrowser as GB
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
	results <- sequence $ tail [ignored
		, qc "Individual Disk <-> JSON" (diskSensible @Individual)
		, qc "PatternCell <-> JSON" (jsonSensible @PatternCell)
		, qc "PatternMetadata <-> JSON" (jsonSensible @PatternMetadata)
		, qc "ConvolutionSize <-> JSON" (jsonSensible @ConvolutionSize)
		, qc "Replication <-> JSON" (jsonSensible @(Replication Int))
		, qc "PatternMetadata <-> JSONKey" (jsonKeySensible @PatternMetadata)
		, qc "ConvolutionSize <-> JSONKey" (jsonKeySensible @ConvolutionSize)
		, qc "Replication <-> JSONKey" (jsonKeySensible @(Replication Bool))
		, qc "toRectangle @Int" \x xss_ -> let xss = toRectangle (x :: Int) xss_ in all (\xs -> length xs == length (V.head xss)) xss
		, qc "newPatternCell/pcNormalize" \s -> pcNormalize (PatternCell s) === newPatternCell s
		, qc "pcNormalize/pcAllDisjuncts" \c -> all (`elem` pcAllDisjuncts) (pcAllowed (pcNormalize c))
		, qc "ByteString <-> printable Text" \bs -> decodePrintable (encodePrintable bs) === BS.dropWhileEnd (0==) bs
		, qc "printable Text <-> ByteString" \(SingleBytes t) -> encodePrintable (decodePrintable t) === t
		, qc "strictlyAscending/sort" $ strictlyAscending . V.fromList . map head . group . sort . id @[Int]
		, qc "strictlyAscending/replicate" $ ruinsStrictAscension @Int (replicate 2)
		, qc "strictlyAscending/minus" $ ruinsStrictAscension @Int (\n -> [n, n-1])
		-- TODO: ptToText >=> ptFromText === pure
		-- you may like arbitraryPatternsTemplate if you decide to complete this TODO
		, qc "PatternMetadata <-> (,)" \pm -> pmFromTuple (pmToTuple pm) === pm
		, qc "ConvolutionSize <-> (,)" \cs -> csFromTuple (csToTuple cs) === cs
		, qc "Replication <-> (,)" \r -> rFromTuple (rToTuple r) == (r :: Replication Int)
		, qc "(,) <-> PatternMetadata" \t -> pmToTuple (pmFromTuple t) === t
		, qc "(,) <-> ConvolutionSize" \t -> csToTuple (csFromTuple t) === t
		, qc "(,) <-> Replication" \t -> rToTuple (rFromTuple t :: Replication Int) === t
		, qc "uiTryAdvance allows legal lock without lookahead match" unitUiTryAdvanceAllowsLegalLock
		, qc "uiTryAdvance rejects illegal lock" unitUiTryAdvanceRejectsIllegalLock
		, qc "uiTryAdvance legal equals uiAdvance" unitUiTryAdvanceMatchesAdvanceOnLegal
		, qc "uiAdvance appends novel variation at end" unitUiAdvanceAppendsVariationAtEnd
		, qc "uiAdvance edge-case activeVariations" unitUiAdvanceEdgeCaseActiveVariations
		, qc "uiAdvance split preserves sibling variations" unitUiAdvanceSplitPreservesSiblingVariations
		, qc "uiVisitAddress can jump to sibling child" unitUiVisitAddressSiblingChild
		, smallQC 5 "Population Disk <-> Browsing" (repurposeuuRoundtrips @Disk @Population @Browsing)
		, smallQC 5 "Population Browsing <-> Disk" (repurposeuuRoundtrips @Browsing @Population @Disk)
		, smallQC 5 "Shared Disk <-> Browsing" (repurposeuuRoundtrips @Disk @Shared @Browsing)
		, smallQC 5 "Shared Browsing <-> Disk" (repurposeuuRoundtrips @Browsing @Shared @Disk)
		, smallQC 5 "Individual Disk <-> Browsing" (repurposeeuRoundtrips @Browsing (arbitraryIndividualDisk . sbParameterCount))
		, smallQC 5 "Individual Browsing <-> Disk" (repurposeueRoundtrips @Disk arbitraryIndividualBrowsing)
		, smallQC 7 "Population Disk <-> JSON" (diskSensible @Population)
		, smallQC 7 "Shared Disk <-> JSON" (diskSensible @Shared)
		]
	unless (all isSuccess results) exitFailure

smallQC :: Testable prop => Int -> String -> prop -> IO QC.Result
smallQC n nm prop = putStrLn nm >> quickCheckWithResult stdArgs { maxSize = n } prop

qc :: Testable prop => String -> prop -> IO QC.Result
qc = smallQC 100

unitUiTryAdvanceAllowsLegalLock :: Property
unitUiTryAdvanceAllowsLegalLock =
	isJust (GB.uiTryAdvance (Lock legalPill) (def :: GB.UIModel)) === True
	where
	legalPill = Pill
		{ content = PillContent Horizontal Blue Red
		, bottomLeftPosition = Position 0 0
		}

unitUiTryAdvanceRejectsIllegalLock :: Property
unitUiTryAdvanceRejectsIllegalLock =
	isNothing (GB.uiTryAdvance (Lock illegalPill) (def :: GB.UIModel)) === True
	where
	illegalPill = Pill
		{ content = PillContent Horizontal Blue Red
		, bottomLeftPosition = Position (-1) 0
		}

unitUiTryAdvanceMatchesAdvanceOnLegal :: Property
unitUiTryAdvanceMatchesAdvanceOnLegal =
	GB.uiTryAdvance edit ui === Just (GB.uiAdvance edit ui)
	where
	ui = def :: GB.UIModel
	edit = GenerateLevel 0x2222 0

unitUiAdvanceAppendsVariationAtEnd :: Property
unitUiAdvanceAppendsVariationAtEnd =
	counterexample (show (GB.nodes ui3)) $
		editAtPath (Seq.singleton 2) 0 ui3 === Just e3
	where
	e1 = GenerateLevel 0x2222 0
	e2 = GenerateLevel 0x3333 0
	e3 = GenerateLevel 0x4444 0
	ui1 = GB.uiAdvance e1 (def :: GB.UIModel)
	ui2 = GB.uiAdvance e2 (GB.setSelection ui1 def)
	ui3 = GB.uiAdvance e3 (GB.setSelection ui2 def)

	editAtPath is i ui = do
		mt <- GB.indexVariations (GB.nodes ui) is
		(e, _gs) <- GB.mainSequence mt Seq.!? i
		pure e

unitUiAdvanceEdgeCaseActiveVariations :: Property
unitUiAdvanceEdgeCaseActiveVariations =
	counterexample (show observed) (observed === expected)
	where
	a = GenerateLevel 0x2222 0
	b = GenerateLevel 0x3333 0
	c = GenerateLevel 0x4444 0
	d = GenerateLevel 0x5555 0

	leaf e = GB.MoveTree (Seq.singleton e) Seq.empty
	root ms vs = GB.MoveTree (Seq.fromList ms) (Seq.fromList (leaf <$> vs))
	ui mt = (def :: GB.UIModel)
		{ GB.nodes = GB.applyEdits mt
		, GB.moveSelection = def
		}
	advance m = GB.activeVariations . GB.uiAdvance m

	observed =
		[ advance a (ui (root [] [a, b]))   -- AlreadyInVariation 0 (out of 2)
		, advance b (ui (root [] [a, b]))   -- AlreadyInVariation 1 (out of 2)
		, advance c (ui (root [a, b] []))   -- SplitAndInserted
		, advance c (ui (root [] [a, b]))   -- Inserted 2
		, advance d (ui (root [a] [b, c]))
			{ GB.activeVariations = Just (GB.ActiveVariations 0 mempty)
			}
		]
	expected =
		[ Just (GB.ActiveVariations 0 mempty)
		, Just (GB.ActiveVariations 1 mempty)
		, Just (GB.ActiveVariations 1 mempty)
		, Just (GB.ActiveVariations 2 mempty)
		, Just (GB.ActiveVariations 1 (IM.singleton 0 (GB.ActiveVariations 0 mempty)))
		]

unitUiAdvanceSplitPreservesSiblingVariations :: Property
unitUiAdvanceSplitPreservesSiblingVariations =
	counterexample (show (GB.nodes ui')) $
		rootHeads === [b, z] .&&. splitNeighborHeads === [x, y]
	where
	a = GenerateLevel 0x2222 0
	b = GenerateLevel 0x3333 0
	x = GenerateLevel 0x4444 0
	y = GenerateLevel 0x5555 0
	z = GenerateLevel 0x6666 0
	leaf e = GB.MoveTree (Seq.singleton e) Seq.empty
	root = GB.MoveTree (Seq.fromList [a, b]) (Seq.fromList [leaf x, leaf y])
	ui = (def :: GB.UIModel)
		{ GB.nodes = GB.applyEdits root
		, GB.moveSelection = def { GB.mainSequenceIndex = 0, GB.variationDepth = 0 }
		}
	ui' = GB.uiAdvance z ui
	rootHeads = branchHeads (GB.nodes ui')
	splitNeighborHeads = case toList (GB.variations (GB.nodes ui')) of
		v0:_ -> branchHeads v0
		[] -> []

	branchHeads mt = catMaybes do
		v <- toList (GB.variations mt)
		pure do
			(e, _gs) <- GB.mainSequence v Seq.!? 0
			pure e

unitUiVisitAddressSiblingChild :: Property
unitUiVisitAddressSiblingChild =
	counterexample (show mres) $
		fmap GB.uiActivePath mres === Just (Seq.fromList [1,0])
		.&&.
		fmap GB.uiMainSequenceIndex mres === Just 0
	where
	a = GenerateLevel 0x2222 0
	b = GenerateLevel 0x3333 0
	x = GenerateLevel 0x4444 0
	y = GenerateLevel 0x5555 0
	leaf e = GB.MoveTree (Seq.singleton e) Seq.empty
	root = GB.MoveTree (Seq.fromList [a]) (Seq.fromList
		[ GB.MoveTree (Seq.singleton b) (Seq.fromList [leaf x])
		, GB.MoveTree (Seq.singleton b) (Seq.fromList [leaf y])
		])
	ui = (def :: GB.UIModel)
		{ GB.nodes = GB.applyEdits root
		, GB.activeVariations = Just (GB.ActiveVariations 0 (IM.singleton 0 (GB.ActiveVariations 0 mempty)))
		, GB.moveSelection = def
		}
	mres = GB.uiVisitAddress ui (GB.MoveTreeAddress (Seq.fromList [1,0]) 0)

repurposeeeRoundtrips :: forall b f a envAB envBA. (Repurpose b a f, Repurpose a b f, Eq (f a), envAB ~ RepurposingEnvironment b a f, envBA ~ RepurposingEnvironment a b f) => (envAB -> envBA -> QC.Gen (f a)) -> envAB -> envBA -> QC.Gen Bool
repurposeeeRoundtrips mkA envAB envBA = mkA envAB envBA <&> \fa -> repurpose envBA (repurpose @b envAB fa) == fa

repurposeeuRoundtrips :: forall b f a envAB. (Repurpose b a f, Repurpose a b f, RepurposingEnvironment a b f ~ (), Eq (f a), envAB ~ RepurposingEnvironment b a f) => (envAB -> QC.Gen (f a)) -> envAB -> QC.Gen Bool
repurposeeuRoundtrips mkA envAB = repurposeeeRoundtrips @b (const . mkA) envAB ()

repurposeueRoundtrips :: forall b f a envBA. (Repurpose b a f, Repurpose a b f, RepurposingEnvironment b a f ~ (), envBA ~ RepurposingEnvironment a b f, Eq (f a)) => (envBA -> QC.Gen (f a)) -> envBA -> QC.Gen Bool
repurposeueRoundtrips mkA = repurposeeeRoundtrips @b (const mkA) ()

repurposeuuRoundtrips :: forall b f a. (Repurpose b a f, Repurpose a b f, RepurposingEnvironment b a f ~ (), RepurposingEnvironment a b f ~ (), Eq (f a)) => f a -> QC.Gen Bool
repurposeuuRoundtrips a = repurposeeeRoundtrips @b (\_ _ -> pure a) () ()

jsonRoundtrips :: (FromJSON a, ToJSON a, Eq a, Show a) => a -> Property
jsonRoundtrips a = decode (encode a) === Just a

jsonEncodingMatches :: (ToJSON a, Show a) => a -> Property
jsonEncodingMatches a = decode (encode a) === Just (toJSON a)

-- | @jsonRoundtrips .&&. jsonEncodingMatches@, but maybe a little more efficient
jsonSensible :: (FromJSON a, ToJSON a, Eq a, Show a) => a -> Property
jsonSensible a = decode encoding === Just a .&&. decode encoding === Just (toJSON a) where
	encoding = encode a

diskSensible :: (f Disk ~ a, FromJSON a, ToJSON a, Eq a, Show a) => a -> Property
diskSensible = jsonSensible

jsonKeyRoundtrips :: (FromJSONKey a, ToJSONKey a, Eq a, Show a) => a -> Property
jsonKeyRoundtrips = case (toJSONKey, fromJSONKey) of
	(ToJSONKeyValue toValue toEncoding, FromJSONKeyValue fromValue) -> \a -> case parse fromValue (toValue a) of
		A.Success a' -> a === a'
		A.Error s -> counterexample s False
	(ToJSONKeyValue{}, _) -> mismatch
	(ToJSONKeyText toKey toEncoding, _) -> case fromJSONKey of
		FromJSONKeyCoerce -> k (pure . coerce)
		FromJSONKeyText f -> k (pure . f)
		FromJSONKeyTextParser f -> k f
		FromJSONKeyValue _ -> mismatch
		where
		k parser a = case parse parser (toText (toKey a)) of
			A.Success a' -> a === a'
			A.Error s -> counterexample s False
	where mismatch = counterexample "encode/decode mismatch" . const False

jsonKeyEncodingMatches :: (ToJSONKey a, Show a) => a -> Property
jsonKeyEncodingMatches = case toJSONKey of
	ToJSONKeyValue toValue toEncoding -> \a -> decode (encodingToLazyByteString (toEncoding a)) === Just (toValue a)
	ToJSONKeyText toKey toEncoding -> \a -> text (toText (toKey a)) === toEncoding a

-- The type of (.&&.) would let us eta contract, but that would generate twice
-- as many inputs as necessary.
jsonKeySensible :: (Arbitrary a, FromJSONKey a, ToJSONKey a, Eq a, Show a) => a -> Property
jsonKeySensible a = jsonKeyRoundtrips a .&&. jsonKeyEncodingMatches a

ruinsStrictAscension :: Ord a => (a -> [a]) -> Int -> [a] -> Property
ruinsStrictAscension ruin n xs = hay xs ==> prop xs .&&. (prop . map head . group . sort) xs where
	tweak xs' = b ++ ruin h ++ e where (b, h:e) = splitAt (n `mod` length xs') xs'
	prop = not . strictlyAscending . V.fromList . tweak

-- TODO: test that lerp and repurpose commute
lerpBoundaries :: (Lerp a, Eq a, Show a) => a -> a -> Property
lerpBoundaries a b = lerp 0 a b === a .&&. lerp 1 a b === b

instance Arbitrary ByteString where
	arbitrary = BS.pack <$> arbitrary
	shrink = shrinkMap BS.pack BS.unpack

instance Arbitrary Text where
	arbitrary = T.pack <$> arbitrary
	shrink = shrinkMap T.pack T.unpack

newtype SingleBytes = SingleBytes Text deriving (Eq, Ord, Read, Show)
instance Arbitrary SingleBytes where
	arbitrary = arbitrary <&> id
		. SingleBytes
		. T.pack
		. map toEnum
		. reverse
		. dropWhile (32==)
		. filter (`S.member` singleBytes)
		where
		singleBytes = S.fromList [32..126] S.\\ S.fromList [34, 92]
	shrink (SingleBytes t) = id
		. map (SingleBytes . T.pack . map toEnum)
		. shrinkList shrinkSingleByte
		. map fromEnum
		. T.unpack
		$ t
		where
		shrinkSingleByte 32 = []
		shrinkSingleByte 35 = [32, 33]
		shrinkSingleByte 93 = [32, 91]
		shrinkSingleByte other = [32, other - 1]

instance Arbitrary a => Arbitrary (Vector a) where
	arbitrary = V.fromList <$> arbitrary
	shrink = shrinkMap V.fromList V.toList

instance Arbitrary (Shared Disk) where
	arbitrary = repurpose' <$> arbitrary @(Shared Browsing)
	shrink = shrinkMap repurpose' (repurpose' @Browsing)

instance Arbitrary a => Arbitrary (Replication a) where
	arbitrary = rFromTuple <$> arbitrary
	shrink = shrinkMap rFromTuple rToTuple

instance Arbitrary (Shared Browsing) where
	arbitrary = do
		patterns <- arbitraryMap \_rep -> arbitraryMapPositive arbitraryPatternTemplateBrowsings
		statistics <- arbitrary
		pure SharedBrowsing
			{ sbPatterns = patterns
			, sbStatisticNames = statistics
			}
	shrink sb = concat . transpose $ tail [ignored
		, [sb { sbStatisticNames = names } | names <- shrink (sbStatisticNames sb)]
		, [sb { sbPatterns = patterns } | patterns <- shrinkMapContainer (shrinkMapContainer (shrinkSetPositive def)) (sbPatterns sb)]
		]

-- This wildcard stands for PatternsTemplate, which isn't in scope. There's at
-- least three solutions: import PatternsTemplate, change the type to one of
-- our data families with a PatternsTemplate field, or delete this method. I
-- don't want to delete because it's likely to be useful for a test that
-- ptToText and ptFromText roundtrip, but since it's not being used for that
-- yet, it's not really possible to decide between importing and retyping.
arbitraryPatternsTemplate :: QC.Gen (IO _)
arbitraryPatternsTemplate = do
	size <- getSize
	cs <- arbitrary
	-- There's a size limit of 0xff in the C++ code.
	ptFromSet cs <$> resize (min size 0xff) (arbitraryPatternTemplateBrowsings cs)

arbitraryPatternTemplateBrowsings :: ConvolutionSize -> QC.Gen (Set (PatternTemplate Browsing))
arbitraryPatternTemplateBrowsings cs = do
	size <- max 1 <$> getSize
	Positive n <- arbitrary
	S.fromList <$> replicateM (min n size) (arbitraryPatternTemplateBrowsing cs)

arbitraryPatternTemplateBrowsing :: ConvolutionSize -> QC.Gen (PatternTemplate Browsing)
arbitraryPatternTemplateBrowsing cs = PatternTemplateBrowsing <$> V.replicateM (csHeight cs) (V.replicateM (csWidth cs) arbitrary)

instance Arbitrary (PatternTemplate Browsing) where
	arbitrary = arbitraryPatternTemplateBrowsing =<< arbitrary
	shrink (PatternTemplateBrowsing cells) = map PatternTemplateBrowsing $ []
		++ [V.take halfh cells | halfh < h]
		++ [V.drop halfh cells | h - halfh < h]
		++ [fmap (V.take halfw) cells | halfw < w]
		++ [fmap (V.drop halfw) cells | w - halfw < w]
		++ [delete i cells | i <- [0..h-1]]
		++ [fmap (delete i) cells | i <- [0..w-1]]
		++ traverse (traverse shrink) cells
		where
		h = V.length cells
		w = case h of
			0 -> 0
			_ -> V.length (V.head cells)
		halfh = h `quot` 2
		halfw = w `quot` 2
		delete i v = V.take i v <> V.drop (i+1) v

instance Arbitrary (Population Disk) where
	arbitrary = do
		generation <- arbitrary
		sharedBrowsing <- arbitrary
		individuals <- arbitraryIndividualDisks (sbParameterCount sharedBrowsing)
		pure PopulationDisk
			{ pdGeneration = generation
			, pdShared = repurpose' sharedBrowsing
			, pdIndividuals = individuals
			}
	-- TODO: shrink while maintaining invariants, seems annoying

arbitraryIndividualDisks :: Int -> QC.Gen (IndexedBy IndividualIndex (Individual Disk))
arbitraryIndividualDisks parameterCount = do
	NonNegative populationSize <- arbitrary
	V.replicateM populationSize (arbitraryIndividualDisk parameterCount)

arbitraryIndividualDisk :: Int -> QC.Gen (Individual Disk)
arbitraryIndividualDisk parameterCount = IndividualDisk <$> V.replicateM parameterCount arbitrary

instance Arbitrary (Population Browsing) where
	arbitrary = do
		generation <- arbitrary
		sb <- arbitrary
		individuals <- arbitraryIndividualBrowsings sb
		pure PopulationBrowsing
			{ pbGeneration = generation
			, pbShared = sb
			, pbIndividuals = individuals
			}

arbitraryIndividualBrowsings :: Shared Browsing -> QC.Gen (IndexedBy IndividualIndex (Individual Browsing))
arbitraryIndividualBrowsings sb = do
	NonNegative populationSize <- arbitrary
	V.replicateM populationSize (arbitraryIndividualBrowsing sb)

arbitraryIndividualBrowsing :: Shared Browsing -> QC.Gen (Individual Browsing)
arbitraryIndividualBrowsing = liftA2 (liftA2 IndividualBrowsing) arbitrarySingleVirusBrowsing arbitrarySingleVirusBrowsing

arbitrarySingleVirusBrowsing :: Shared Browsing -> QC.Gen (SingleVirus Browsing)
arbitrarySingleVirusBrowsing sb = do
	position <- V.replicateM (sbPatternCount sb) arbitrary
	move <- V.replicateM (sbPatternCount sb) arbitrary
	statistics <- V.replicateM (sbStatisticCount sb) arbitrary
	pure SingleVirusBrowsing
		{ svbPosition = position
		, svbMove = move
		, svbStatistics = statistics
		}

instance Arbitrary (Individual Disk) where
	arbitrary = IndividualDisk <$> arbitrary
	shrink = shrinkMap IndividualDisk idParameters

instance Arbitrary PatternCell where
	arbitrary = newPatternCell <$> arbitrary
	shrink = shrinkMap PatternCell pcAllowed

instance Arbitrary a => Arbitrary (WithSentinels a) where
	arbitrary = do
		n <- chooseInt (0, 20)
		case n of
			0 -> pure EmptySentinel
			1 -> pure OutOfBoundsSentinel
			_ -> NonSentinel <$> arbitrary
	shrink = \case
		EmptySentinel -> []
		OutOfBoundsSentinel -> [EmptySentinel]
		NonSentinel a -> [EmptySentinel, OutOfBoundsSentinel] ++ map NonSentinel (shrink a)

instance Arbitrary PatternMetadata where
	arbitrary = liftA2 PatternMetadata arbitrary arbitrary
	shrink = shrinkMap pmFromTuple pmToTuple

instance Arbitrary ConvolutionSize where
	arbitrary = liftA2 ConvolutionSize max16 max16 where
		max16 = arbitrary <&> \x -> 1 + x `mod` 16
	shrink cs@(ConvolutionSize w h) = []
		++ [ConvolutionSize 1 h | w /= 1]
		++ [ConvolutionSize w 1 | h /= 1]
		++ [ConvolutionSize w (h-1) | h > 1]
		++ [ConvolutionSize (w-1) h | w > 1]

newtype BoundedEnum a = BoundedEnum a
instance (Bounded a, Enum a, Eq a) => Arbitrary (BoundedEnum a) where
	arbitrary = coerce (arbitraryBoundedEnum @a)
	shrink = coerce (shrinkBoundedEnum @a)

deriving via BoundedEnum Color instance Arbitrary Color
deriving via BoundedEnum Shape instance Arbitrary Shape

arbitraryMap :: (Arbitrary k, Ord k) => (k -> QC.Gen a) -> QC.Gen (Map k a)
arbitraryMap f = sequence . M.fromSet f =<< arbitrary

arbitraryMapPositive :: (Arbitrary k, Ord k) => (k -> QC.Gen a) -> QC.Gen (Map k a)
arbitraryMapPositive f = sequence . M.fromSet f =<< arbitrarySetPositive

arbitrarySetPositive :: (Arbitrary a, Ord a) => QC.Gen (Set a)
arbitrarySetPositive = do
	s <- arbitrary
	if S.null s then arbitrarySetPositive else pure s

shrinkMapContainer :: Ord k => (v -> [v]) -> Map k v -> [Map k v]
shrinkMapContainer f = map M.fromList . shrinkList (traverse f) . M.toList

shrinkSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSet f = map S.fromList . shrinkList f . S.toList

shrinkListPositive :: (a -> [a]) -> [a] -> [[a]]
shrinkListPositive f = filter hay . shrinkList f

shrinkSetPositive :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSetPositive f = map S.fromList . shrinkListPositive f . S.toList
