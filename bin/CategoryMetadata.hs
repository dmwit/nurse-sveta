module CategoryMetadata where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Encoding
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data LevelMetric = LevelMetric
	{ lmMetric :: Int
	, lmSource :: FilePath
	} deriving (Eq, Ord, Read, Show)

lmToTuple :: LevelMetric -> (Int, FilePath)
lmToTuple = liftA2 (,) lmMetric lmSource

lmFromTuple :: (Int, FilePath) -> LevelMetric
lmFromTuple = uncurry LevelMetric

instance FromJSON LevelMetric where parseJSON = fmap lmFromTuple . parseJSON
instance ToJSON LevelMetric where
	toJSON = toJSON . lmToTuple
	toEncoding lm = list id [toEncoding (lmMetric lm), toEncoding (lmSource lm)]

lmFewest :: LevelMetric -> LevelMetric -> LevelMetric
lmFewest lm lm' = if lmMetric lm < lmMetric lm' then lm else lm'

lmMost :: LevelMetric -> LevelMetric -> LevelMetric
lmMost lm lm' = if lmMetric lm > lmMetric lm' then lm else lm'

lmSum :: LevelMetric -> LevelMetric -> LevelMetric
lmSum lm lm' = LevelMetric
	{ lmMetric = lmMetric lm + lmMetric lm'
	, lmSource = "<all>"
	}

lmNeither :: LevelMetric -> LevelMetric -> LevelMetric
lmNeither _lm _lm' = LevelMetric
	{ lmMetric = -1
	, lmSource = "<none>"
	}

lmFloat :: LevelMetric -> Float
lmFloat = fromIntegral . lmMetric

data CategoryMetrics = CategoryMetrics
	{ cmVirusesKilled :: IntMap LevelMetric
	, cmFramesToWin   :: IntMap LevelMetric
	, cmFramesToLoss  :: IntMap LevelMetric
	, cmFrames        :: IntMap LevelMetric
	} deriving (Eq, Ord, Read, Show)

vkFieldName, ftwFieldName, ftlFieldName :: Key
vkFieldName = "viruses killed"
ftwFieldName = "frames to win"
ftlFieldName = "frames to loss"
frFieldName = "frames"

instance ToJSON CategoryMetrics where
	toJSON cm = object $ tail [undefined
		,  vkFieldName .= cmVirusesKilled cm
		, ftwFieldName .= cmFramesToWin cm
		, ftlFieldName .= cmFramesToLoss cm
		,  frFieldName .= cmFrames cm
		]
	toEncoding cm = pairs $ mempty
		<>  vkFieldName .= cmVirusesKilled cm
		<> ftwFieldName .= cmFramesToWin cm
		<> ftlFieldName .= cmFramesToLoss cm
		<>  frFieldName .= cmFrames cm

instance FromJSON CategoryMetrics where
	parseJSON = withObject "CategoryMetrics" $ \v -> pure CategoryMetrics
		<*> v .:  vkFieldName
		<*> v .: ftwFieldName
		<*> v .: ftlFieldName
		<*> v .:  frFieldName

newCategoryMetrics :: CategoryMetrics
newCategoryMetrics = CategoryMetrics
	{ cmVirusesKilled = mempty
	, cmFramesToWin   = mempty
	, cmFramesToLoss  = mempty
	, cmFrames        = mempty
	}

-- invariant: corresponding fields in cmBest and cmLatest have the same keys
data CategoryMetadata = CategoryMetadata
	{ cmBest :: CategoryMetrics
	, cmLatest :: CategoryMetrics
	, cmCumulative :: CategoryMetrics
	} deriving (Eq, Ord, Read, Show)

bFieldName, rFieldName, cFieldName :: Key
bFieldName = "best"
rFieldName = "recent"
cFieldName = "cumulative"

instance ToJSON CategoryMetadata where
	toJSON cm = object $ tail [undefined
		, bFieldName .= cmBest cm
		, rFieldName .= cmLatest cm
		, cFieldName .= cmCumulative cm
		]
	toEncoding cm = pairs $ mempty
		<> bFieldName .= cmBest cm
		<> rFieldName .= cmLatest cm
		<> cFieldName .= cmCumulative cm

instance FromJSON CategoryMetadata where
	parseJSON = withObject "CategoryMetadata" $ \v -> pure CategoryMetadata
		<*> v .: bFieldName
		<*> v .: rFieldName
		<*> v .: cFieldName

newCategoryMetadata :: CategoryMetadata
newCategoryMetadata = CategoryMetadata
	{ cmBest = newCategoryMetrics
	, cmLatest = newCategoryMetrics
	, cmCumulative = newCategoryMetrics
	}

cmInsert :: FilePath -> Int -> Int -> Int -> CategoryMetadata -> CategoryMetadata
cmInsert fp startingViruses virusesKilled frames CategoryMetadata { cmBest = b, cmLatest = r, cmCumulative = c } = CategoryMetadata
	{ cmBest       = mkMetrics lmMost lmFewest lmMost lmNeither b
	, cmLatest     = mkMetrics const  const    const  const     r
	, cmCumulative = mkMetrics lmSum  lmSum    lmSum  lmSum     c
	} where
	mkMetrics vk fw fl fr cm = CategoryMetrics
		{ cmVirusesKilled = iw vk True   virusesKilled cmVirusesKilled cm
		, cmFramesToWin   = iw fw isWin  frames        cmFramesToWin   cm
		, cmFramesToLoss  = iw fl isLoss frames        cmFramesToLoss  cm
		, cmFrames        = iw fr True   frames        cmFrames        cm
		}
	isWin = startingViruses == virusesKilled
	isLoss = not isWin
	iw comb cond metric field record = (if cond then IM.insertWith comb startingViruses (LevelMetric metric fp) else id) (field record)
