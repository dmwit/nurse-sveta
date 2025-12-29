{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}

import Ms.Mendel
import Test.QuickCheck

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Test.QuickCheck as QC

repurposeRoundtrips :: forall f a b. (Repurpose f a b, Repurpose f b a, Eq (f a)) => RepurposingEnvironment f a b -> RepurposingEnvironment f b a -> f a -> Bool
repurposeRoundtrips envAB envBA fa = repurpose envBA (repurpose @_ @_ @b envAB fa) == fa

repurpose_Roundtrips :: forall f a b. (Repurpose f a b, Repurpose f b a, RepurposingEnvironment f a b ~ (), RepurposingEnvironment f b a ~ (), Eq (f a)) => f a -> Bool
repurpose_Roundtrips = repurposeRoundtrips @_ @_ @b () ()

repurposeIORoundtrips :: forall f a b. (RepurposeIO f a b, RepurposeIO f b a, Eq (f a)) => RepurposingEnvironmentIO f a b -> RepurposingEnvironmentIO f b a -> f a -> Property
repurposeIORoundtrips envAB envBA fa = idempotentIOProperty $ (fa==) <$> (repurposeIO envBA =<< repurposeIO @_ @_ @b envAB fa)

repurposeIO_Roundtrips :: forall f a b. (RepurposeIO f a b, RepurposeIO f b a, RepurposingEnvironmentIO f a b ~ (), RepurposingEnvironmentIO f b a ~ (), Eq (f a)) => f a -> Property
repurposeIO_Roundtrips = repurposeIORoundtrips @_ @_ @b () ()

prop_SharedDiskBrowsingRoundtrips :: Shared Disk -> Bool
prop_SharedDiskBrowsingRoundtrips = repurpose_Roundtrips @_ @_ @Browsing

prop_SharedBrowsingDiskRoundtrips :: Shared Browsing -> Bool
prop_SharedBrowsingDiskRoundtrips = repurpose_Roundtrips @_ @_ @Disk

pure []

main :: IO ()
main = do
	success <- $quickCheckAll
	unless success exitFailure

instance Arbitrary Text where
	arbitrary = T.pack <$> arbitrary
	shrink = shrinkMap T.pack T.unpack

instance Arbitrary a => Arbitrary (Vector a) where
	arbitrary = V.fromList <$> arbitrary
	shrink = shrinkMap V.fromList V.toList

instance Arbitrary (Shared Disk) where
	arbitrary = repurpose_ <$> arbitrary @(Shared Browsing)
	shrink = shrinkMap repurpose_ (repurpose_ @_ @_ @Browsing)

instance Arbitrary a => Arbitrary (Replication a) where
	arbitrary = rFromTuple <$> arbitrary
	shrink = shrinkMap rFromTuple rToTuple

instance Arbitrary (Shared Browsing) where
	arbitrary = do
		patterns <- arbitraryMap \_rep -> arbitraryMap makePatterns
		statistics <- arbitrary
		pure SharedBrowsing
			{ sbPatterns = patterns
			, sbStatisticNames = statistics
			}
		where
		makePatterns :: ConvolutionSize -> QC.Gen (Set (PatternTemplate Browsing))
		makePatterns cs = do
			Positive n <- arbitrary
			S.fromList <$> replicateM n (arbitraryPatternTemplateBrowsing cs)
	shrink sb = concat . transpose $ tail [ignored
		, [sb { sbStatisticNames = names } | names <- shrink (sbStatisticNames sb)]
		, [sb { sbPatterns = patterns } | patterns <- shrinkMapContainer (shrinkMapContainer (shrinkSetPositive def)) (sbPatterns sb)]
		]

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

instance Arbitrary ConvolutionSize where
	arbitrary = liftA2 (\w h -> ConvolutionSize
		{ csWidth = getPositive w
		, csHeight = getPositive h
		}) arbitrary arbitrary
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

shrinkMapContainer :: Ord k => (v -> [v]) -> Map k v -> [Map k v]
shrinkMapContainer f = map M.fromList . shrinkList (traverse f) . M.toList

shrinkSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSet f = map S.fromList . shrinkList f . S.toList

shrinkListPositive :: (a -> [a]) -> [a] -> [[a]]
shrinkListPositive f = filter hay . shrinkList f

shrinkSetPositive :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSetPositive f = map S.fromList . shrinkListPositive f . S.toList
