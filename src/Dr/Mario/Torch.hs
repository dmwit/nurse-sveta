{-# LANGUAGE AllowAmbiguousTypes #-}
{-# Language DataKinds #-}

module Dr.Mario.Torch (
	NurseSvetaNetDef, netDefForwardSS, netDefForwardST, netDefForwardTT,
	NurseSvetaNetSpec(..), nurseSvetaNetSpec, nurseSvetaNetSpecDef, NurseSvetaNet, netForward,
	NetInputStruct(..), NetInputTensor(..), nisToTensor,
	NetOutputStruct(..), NetOutputTensor(..), notToStruct,
	ResidualSpec(..), Residual, residualForward,
	StableSquareConvSpec(..), ConstSquareConvSpec, stableSpec, constChansSpec,
	StableSquareConv, ConstSquareConv,
	Conv2dReLUInitSpec(..), Conv2dReLUInit,
	BatchNormSpec(..), BatchNorm, bnForward,
	unsafeAsTensor,
	KnownConfig, KnownAndValidRand, BatchNormDTypeIsValid, CPU0
	) where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Proxy
import Data.Type.Equality
import Dr.Mario.Model
import Dr.Mario.Model.Internal
import GHC.Exts
import GHC.TypeLits
import Torch.Typed hiding (sqrt)
import Unsafe.Coerce

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Sized as VS
import qualified Dr.Mario.Model as DM
import qualified Torch.Typed as T
import qualified Torch as TU
import qualified Torch.Layout as TU

type CPU0 = '(CPU, 0)

unsafeAsTensor :: TU.TensorLike a => a -> Tensor dev dtype shape
unsafeAsTensor = UnsafeMkTensor . TU.asTensor

newtype Conv2dReLUInitSpec (iChans :: Nat) (oChans :: Nat) (kw :: Nat) (kh :: Nat) (dtype :: DType) (dev :: (DeviceType, Nat)) = Conv2dReLUInitSpec { reluInitLeakage :: Double }
	deriving (Eq, Ord, Read, Show)

newtype Conv2dReLUInit iChans oChans kw kh dtype dev = Conv2dReLUInit
	{ likeConv2d :: Conv2d iChans oChans kw kh dtype dev }
	deriving newtype (Show, Parameterized)

instance (stride ~ '(sx, sy), padding ~ '(pw, ph)
	, All KnownNat [sx, sy, pw, ph, iChans, oChans, kw, kh, iw, ih, n, ow, oh]
	, ConvSideCheck iw kw sx pw ow
	, ConvSideCheck ih kh sy ph oh
	) => HasForward
	(Conv2dReLUInit iChans oChans kw kh dtype dev)
	(Tensor dev dtype [n, iChans, iw, ih], Proxy stride, Proxy padding)
	(Tensor dev dtype [n, oChans, ow, oh])
	where
	forward = forward.likeConv2d
	forwardStoch = forwardStoch.likeConv2d

type KnownConfig shape dtype dev = (All KnownNat shape, KnownDType dtype, KnownDevice dev)
type KnownAndValidRand shape dtype dev = (KnownConfig shape dtype dev, RandDTypeIsValid dev dtype)

-- | Follows advice from Delving Deep into Rectifiers
instance KnownAndValidRand [iChans, oChans, kw, kh] dtype dev => Randomizable
	(Conv2dReLUInitSpec iChans oChans kw kh dtype dev)
	(Conv2dReLUInit     iChans oChans kw kh dtype dev)
	where
	sample spec = do
		w <- makeIndependent . (scaling*) =<< randn
		b <- makeIndependent =<< randn
		pure (Conv2dReLUInit (Conv2d { conv2dWeight = w, conv2dBias = b }))
		where
		scaling = realToFrac $ sqrt
			(2 / ((1 + reluInitLeakage spec) * numParameters))
		numParameters = fromIntegral $ natValI @kw * natValI @kh * natValI @iChans

type ConstSquareConvSpec chans = StableSquareConvSpec chans chans
newtype StableSquareConvSpec iChans oChans p dtype dev = StableSquareConvSpec
	{ stableSquareLeakage :: Conv2dReLUInitSpec iChans oChans (2*p+1) (2*p+1) dtype dev }
	deriving (Eq, Ord, Read, Show)

stableSpec :: Double -> StableSquareConvSpec iChans oChans p dtype dev
stableSpec = StableSquareConvSpec . Conv2dReLUInitSpec

constChansSpec :: Double -> ConstSquareConvSpec chans p dtype dev
constChansSpec = stableSpec

-- | Like 'Conv2dReLUInit', except for two things:
--
-- 1. Its 'forward' takes only a tensor.
-- 2. Its third argument is the padding, not the kernel size. The kernel size
--    is computed from the padding in such a way as to maintain the width and
--    height of each layer, namely, kernel size = 2*padding + 1.
newtype StableSquareConv iChans oChans p dtype dev = StableSquareConv
	{ likeConv2dReLUInit :: Conv2dReLUInit iChans oChans (2*p+1) (2*p+1) dtype dev }
	deriving newtype (Show, Parameterized)
type ConstSquareConv chans = StableSquareConv chans chans

instance KnownAndValidRand [iChans, oChans, 2*p+1] dtype dev => Randomizable
	(StableSquareConvSpec iChans oChans p dtype dev)
	(StableSquareConv     iChans oChans p dtype dev)
	where
	sample (StableSquareConvSpec n) = StableSquareConv <$> sample n

instance (k ~ (2*p+1)
	, All KnownNat [iChans, oChans, k, w, h, n, p]
	, ConvSideCheck w k 1 p w
	, ConvSideCheck h k 1 p h
	) => HasForward
	(StableSquareConv iChans oChans p dtype dev)
	(Tensor dev dtype [n, iChans, w, h])
	(Tensor dev dtype [n, oChans, w, h])
	where
	forward (StableSquareConv c) t = forward c (t, Proxy @'(1,1), Proxy @'(p,p))
	forwardStoch (StableSquareConv c) t = forwardStoch c (t, Proxy @'(1,1), Proxy @'(p,p))

data BatchNormSpec c dtype dev = BatchNormSpec
	{ bnsMomentum :: Double -- ^ the sensible kind, as in most optimizers -- e.g. 0.9ish is sensible
	, bnsEpsilon :: Double
	} deriving (Eq, Ord, Read, Show)

instance KnownConfig '[c] dtype dev => Randomizable (BatchNormSpec c dtype dev) (BatchNorm c dtype dev) where
	sample spec = pure BatchNorm
		<*> makeIndependent ones
		<*> makeIndependent zeros
		<*> newIORef zeros
		<*> newIORef zeros
		<*> pure (expand False (full @'[] (bnsMomentum spec)))
		<*> pure (expand False (full @'[] (bnsEpsilon spec)))

-- The BatchNorm that ships with hasktorch is just broken. See
-- https://github.com/hasktorch/hasktorch/issues/626. Also it doesn't even
-- exist in the typed API (but that's a smaller problem). And it doesn't
-- understand convolutions, i.e. it isn't pytorch's BatchNorm2d.
data BatchNorm c dtype dev = BatchNorm
	{ bnWeight :: Parameter dev dtype '[c]
	, bnBias :: Parameter dev dtype '[c]
	, bnRunningMean :: IORef (Tensor dev dtype '[c])
	, bnRunningVar :: IORef (Tensor dev dtype '[c])
	, bnMomentum :: Tensor dev dtype '[c]
	, bnEpsilon :: Tensor dev dtype '[c]
	}

infixr 4 :::
pattern (:::) :: a -> HList as -> HList (a:as)
pattern a ::: as = HCons (a, as)

instance Parameterized (BatchNorm c dtype dev) where
	type Parameters (BatchNorm c dtype dev) = [Parameter dev dtype '[c], Parameter dev dtype '[c]]
	flattenParameters bn = bnWeight bn ::: bnBias bn ::: HNil
	replaceParameters bn (w ::: b ::: HNil) = bn { bnWeight = w, bnBias = b }

type BatchNormDTypeIsValid dev dtype =
	( KnownDevice dev
	, MeanDTypeValidation dev dtype
	, BasicArithmeticDTypeIsValid dev dtype
	, StandardFloatingPointDTypeValidation dev dtype
	)

bnForward :: forall n dims c dtype dev.
	( BatchNormDTypeIsValid dev dtype
	, KnownShape dims
	, All KnownNat [n, n*Numel dims, c]
	, AllDimsPositive [n*Numel dims, c]
	) =>
	BatchNorm c dtype dev ->
	Bool ->
	Tensor dev dtype (n:c:dims) ->
	IO (Tensor dev dtype (n:c:dims))
bnForward bn training i = unshape <$> bnForwardImpl bn training (shape i) where
	shape = transpose @0 @1 . reshape @[c, n*Numel dims] . transpose @0 @1
	unshape = transpose @0 @1 . reshape @(c:n:dims) . transpose @0 @1

bnForwardImpl :: forall n c dtype dev.
	( BatchNormDTypeIsValid dev dtype
	, AllDimsPositive [n, c]
	) =>
	BatchNorm c dtype dev ->
	Bool ->
	Tensor dev dtype [n, c] ->
	IO (Tensor dev dtype [n, c])
bnForwardImpl bn training i = if training
	then do
		modifyIORef (bnRunningMean bn) (bnAbsorb bn means)
		modifyIORef (bnRunningVar bn) (bnAbsorb bn vars)
		return (compute means vars)
	else do
		runningMean <- readIORef (bnRunningMean bn)
		runningVar <- readIORef (bnRunningVar bn)
		return (compute runningMean runningVar)
	where
	compute mu sigma = (((i `sub` mu) `T.div` T.sqrt (sigma `add` bnEpsilon bn)) `mul` toDependent (bnWeight bn)) `add` toDependent (bnBias bn)
	means = meanDim @0 i
	vars = meanDim @0 ((i `sub` means)^2)

bnAbsorb ::
	KnownDevice dev =>
	BatchNorm c dtype dev ->
	Tensor dev dtype '[c] -> Tensor dev dtype '[c] -> Tensor dev dtype '[c]
bnAbsorb bn new old = bnMomentum bn * old + (1-bnMomentum bn) * new

data ResidualSpec chans p dtype dev = ResidualSpec
	{ rsLeakage :: Double
	, rsBatchNormSpec :: BatchNormSpec chans dtype dev
	} deriving (Eq, Ord, Read, Show)

data Residual chans p dtype dev = Residual
	{ resLeakage :: Double
	, resConv0, resConv1 :: ConstSquareConv chans p dtype dev
	, resNorm0, resNorm1 :: BatchNorm chans dtype dev
	}

instance KnownAndValidRand [chans, 2*p+1] dtype dev => Randomizable
	(ResidualSpec chans p dtype dev)
	(Residual     chans p dtype dev)
	where
	sample ResidualSpec { rsLeakage = leakage, rsBatchNormSpec = bns } = pure Residual
		<*> pure leakage
		<*> sample (constChansSpec leakage)
		<*> sample (constChansSpec leakage)
		<*> sample bns
		<*> sample bns

residualForward :: (k ~ (2*p+1)
	, BatchNormDTypeIsValid dev dtype
	, All KnownNat [chans, k, w, h, n, p, Numel [n,w,h]]
	, AllDimsPositive [Numel [n,w,h], chans]
	, ConvSideCheck w k 1 p w
	, ConvSideCheck h k 1 p h
	) =>
	Residual chans p dtype dev ->
	Bool ->
	Tensor dev dtype [n, chans, w, h] ->
	IO (Tensor dev dtype [n, chans, w, h])
residualForward res training t = do
	let a = forward (resConv0 res) t
	b <- bnForward (resNorm0 res) training a
	let c = leakyRelu (resLeakage res) b
	    d = forward (resConv1 res) c
	e <- bnForward (resNorm1 res) training d
	pure $ leakyRelu (resLeakage res) (t + e)

-- TODO: ideas for other inputs: NES/MWC pill RNG, cost model stats, gravity,
-- NES vs SNES movement, NES vs SNES pill distribution
data NetInputStruct = NetInputStruct
	{ nisBoard :: IOBoard
	, nisLookaheadL :: Color
	, nisLookaheadR :: Color
	}

data NetInputTensor dev where
	NetInputTensor ::
		( KnownNat n
		, KnownNat (PerCell n)
		, 1 <= PerCell n
		) =>
		{ nitBoard :: Tensor dev 'Double [n, 7, 8, 16]
		, nitLookahead :: Tensor dev 'Double [n, 6]
		} -> NetInputTensor dev

class OneHot a where
	toIndex :: a -> Int
	fromIndex :: Int -> a

instance OneHot Color where
	toIndex = fromEnum
	fromIndex = toEnum

instance OneHot DM.Shape where
	toIndex = \case
		Virus -> 0
		East -> 1
		West -> 2
		_ -> 3 -- Disconnected, North, and South all have the same strategic content
	fromIndex = \case
		0 -> Virus
		1 -> East
		2 -> West
		3 -> Disconnected

-- It would be really bad to share the return value of zeros across calls to
-- this function, since we modify it in-place. Hopefully this NoInline pragma
-- will prevent that.
{-# NoInline nisToTensor #-}
nisToTensor :: forall dev. KnownDevice dev => [NetInputStruct] -> IO (NetInputTensor dev)
nisToTensor nis = do
	tBoard <- evaluate (TU.zeros [n, 7, 8, 16] opts)
	tLookahead <- evaluate (TU.zeros [n, 6] opts)
	TU.withTensor tBoard $ \pBoard ->
		TU.withTensor tLookahead $ \pLookahead ->
			for_ (zip [0..] nis) $ \(i, ni) -> do
				assert (mwidth (nisBoard ni) == 8 && mheight (nisBoard ni) == 16)
					$ "expected all boards to have size 8x16, but saw a board with size " ++ show (mwidth (nisBoard ni)) ++ "x" ++ show (mheight (nisBoard ni))
				let iBoard = i*onehotCount
				    iLookahead = i*6
				pokeOne pLookahead (    iLookahead + toIndex (nisLookaheadL ni))
				pokeOne pLookahead (3 + iLookahead + toIndex (nisLookaheadR ni))
				-- IOBoard stores cells in a 1D array with the y coordinate
				-- varying fastest... just like the tensor we want to make.
				-- This means we get to reuse the index into that array as an
				-- index into our tensor. Nice.
				MVU.iforM_ (mcells (nisBoard ni)) $ \j cell -> case cell of
					Empty -> pure ()
					Occupied c s -> do
						-- shiftl x 7 = x * cellCount
						pokeOne pBoard (iBoard + shiftL (    toIndex c) 7 + j)
						pokeOne pBoard (iBoard + shiftL (3 + toIndex s) 7 + j)
	case someNatVal (toInteger n) of
		Just (SomeNat (_ :: Proxy n)) -> withDict (knownMul @n @128) $ case leNat @1 @(PerCell n) of
			Just Dict -> pure $ NetInputTensor @n (UnsafeMkTensor tBoard) (UnsafeMkTensor tLookahead)
			Nothing -> fail "Invalid arguments to nisToTensor: input list must be non-empty"
		Nothing -> fail "The impossible happened in nisToTensor: length returned a negative number, or someNatVal failed on a non-negative number"
	where
	pokeOne p i = TU._pokeElemOff p i (1 :: Double)
	assert b msg = unless b . fail $ "Assertion failure in nisToTensor: " ++ msg

	onehotCount, cellCount :: Int
	onehotCount = 7*cellCount
	cellCount = 8*16

	n = Prelude.length nis
	opts = id
		. TU.withDType Double
		. TU.withDevice (deviceVal @dev)
		. TU.withLayout TU.Strided -- this is the default anyway, but let's defend against that changing for some reason
		$ TU.defaultOpts

-- TODO: ideas for other outputs: what position will the next occurrence of
-- each kind of pill go? will each position be empty at end of game? maybe
-- something about what order the viruses are cleared in, or what frame it
-- happens on?
data NetOutputStruct = NetOutputStruct
	-- TODO: maybe just use Tensors directly instead of vectors here?
	{ nosPriors :: HashMap PillContent (VU.Vector (VU.Vector Double))
	, nosValuation :: Double
	, nosVirusClears :: Double
	, nosPills :: Double
	, nosFrames :: Double
	}

data NetOutputTensor dev where
	NetOutputTensor :: KnownNat n =>
		{ notPriors :: Tensor dev 'Double [n, 4, 8, 16]
		, notValuation :: Tensor dev 'Double '[n]
		, notScalars :: Tensor dev 'Double [n, 3]
		} -> NetOutputTensor dev

notToStruct :: [(Color, Color)] -> NetOutputTensor dev -> [NetOutputStruct]
notToStruct lookaheads NetOutputTensor { notPriors = priors, notValuation = valuation, notScalars = scalars } =
	[ NetOutputStruct
		{ nosPriors = HM.fromList
			[ ( pc
			  , VS.fromSized . VS.generate @8 $ \x ->
			    	VS.fromSized . VS.generate @16 $ \y ->
			    		toDouble (selectIdx @0 (selectIdx @0 (selectIdx @0 tPriors numRots) x) y)
			  )
			| (numRots, pc) <- zip
				[minBound .. maxBound]
				(iterate (`rotateContent` Clockwise) (PillContent Horizontal l r))
			]
		, nosValuation = toDouble (selectIdx @0 valuation i)
		, nosVirusClears = toDouble (select @0 @0 tScalars)
		, nosPills = toDouble (select @0 @1 tScalars)
		, nosFrames = toDouble (select @0 @2 tScalars)
		}
	| (i, (l, r)) <- zip [minBound .. maxBound] lookaheads
	, let tPriors = selectIdx @0 priors i
	      tScalars = selectIdx @0 scalars i
	]

data NurseSvetaNetSpec depth filters p dev = NurseSvetaNetSpec
	{ nsBoardConvolutionSpec :: StableSquareConvSpec 7 filters p 'Double dev
	, nsInputNormalizationSpec :: BatchNormSpec filters 'Double dev
	, nsResidualsSpec :: ResidualSpec filters p 'Double dev
	, nsPriorsConvolutionSpec :: StableSquareConvSpec filters 4 p 'Double dev
	} deriving (Eq, Ord, Read, Show)

nurseSvetaNetSpec :: Double -> BatchNormSpec filters 'Double dev -> NurseSvetaNetSpec depth filters p dev
nurseSvetaNetSpec leakage bns = NurseSvetaNetSpec
	{ nsBoardConvolutionSpec = stableSpec leakage
	, nsInputNormalizationSpec = bns
	, nsResidualsSpec = ResidualSpec
		{ rsLeakage = leakage
		, rsBatchNormSpec = bns
		}
	, nsPriorsConvolutionSpec = stableSpec leakage
	}

nurseSvetaNetSpecDef :: NurseSvetaNetSpecDef dev
nurseSvetaNetSpecDef = nurseSvetaNetSpec 0.01 BatchNormSpec
	{ bnsMomentum = 0.95
	, bnsEpsilon = 1e-5
	}

instance KnownAndValidRand [depth, filters, PerCell filters, 2*p+1] 'Double dev => Randomizable
	(NurseSvetaNetSpec depth filters p dev)
	(NurseSvetaNet     depth filters p dev)
	where
	sample ns = pure NurseSvetaNet
		<*> sample (nsBoardConvolutionSpec ns)
		<*> sample LinearSpec
		<*> sample (nsInputNormalizationSpec ns)
		<*> VS.replicateM (sample (nsResidualsSpec ns))
		<*> sample LinearSpec
		<*> sample LinearSpec
		<*> sample (nsPriorsConvolutionSpec ns)

type PerCell n = Numel [n,8,16]

data NurseSvetaNet depth filters p dev = NurseSvetaNet
	{ netBoardConvolution :: StableSquareConv 7 filters p 'Double dev
	, netInputLinear :: Linear 6 (PerCell filters) 'Double dev
	, netInputNormalization :: BatchNorm filters 'Double dev
	, netResiduals :: VS.Vector depth (Residual filters p 'Double dev)
	, netOutputFraction :: Linear (PerCell filters) 1 'Double dev
	, netOutputPositive :: Linear (PerCell filters) 3 'Double dev
	, netPriorsConvolution :: StableSquareConv filters 4 p 'Double dev
	}

netForward :: forall depth filters p dev k.
	( k ~ (2*p+1)
	, BatchNormDTypeIsValid dev 'Double
	, All KnownNat [filters, p, k, PerCell filters]
	, 1 <= filters
	, ConvSideCheck  8 k 1 p  8
	, ConvSideCheck 16 k 1 p 16
	) =>
	NurseSvetaNet depth filters p dev ->
	Bool ->
	NetInputTensor dev ->
	IO (NetOutputTensor dev)
netForward net training NetInputTensor { nitBoard = board, nitLookahead = lookahead } = do
	let a = forward (netBoardConvolution net) board
	    b = forward (netInputLinear net) lookahead
	    c = a + reshape b
	d <- bnForward (netInputNormalization net) training c
	e <- VS.foldM (\tensor layer -> residualForward layer training tensor) d (netResiduals net)
	let f = forward (netPriorsConvolution net) e
	    g = reshape @[_, PerCell filters] e
	    h = T.sigmoid $ forward (netOutputFraction net) g
	    i = T.exp     $ forward (netOutputPositive net) g
	pure NetOutputTensor
		{ notPriors = f
		, notValuation = reshape h
		, notScalars = i
		}

type NurseSvetaNetSpecDef = NurseSvetaNetSpec 30 64 1
type NurseSvetaNetDef = NurseSvetaNet 30 64 1

netDefForwardTT ::
	BatchNormDTypeIsValid dev 'Double =>
	NurseSvetaNetDef dev ->
	Bool ->
	NetInputTensor dev ->
	IO (NetOutputTensor dev)
netDefForwardTT = netForward

netDefForwardST ::
	BatchNormDTypeIsValid dev 'Double =>
	NurseSvetaNetDef dev ->
	Bool ->
	[NetInputStruct] ->
	IO (NetOutputTensor dev)
netDefForwardST net training = nisToTensor >=> netDefForwardTT net training

netDefForwardSS ::
	BatchNormDTypeIsValid dev 'Double =>
	NurseSvetaNetDef dev ->
	Bool ->
	[NetInputStruct] ->
	IO [NetOutputStruct]
netDefForwardSS net training niss =
	notToStruct [(nisLookaheadL nis, nisLookaheadR nis) | nis <- niss]
	<$> netDefForwardST net training niss

data Dict c where Dict :: c => Dict c

unsafeRefl :: forall a b. Dict (a ~ b)
unsafeRefl = unsafeCoerce (Dict @(a ~ a))

withDict :: Dict c -> (c => r) -> r
withDict Dict r = r

{-# Inline unsafeKnownNat #-}
unsafeKnownNat :: Int -> Dict (KnownNat n)
unsafeKnownNat n = case someNatVal (toInteger n) of
	Just (SomeNat (p :: Proxy n')) -> unsafeCoerce (Dict @(KnownNat n'))

knownMul :: forall m n. (KnownNat m, KnownNat n) => Dict (KnownNat (m*n))
knownMul = unsafeKnownNat (natValI @m * natValI @n)

-- cmpNat isn't available in some of the bases we support
leNat :: forall m n. (KnownNat m, KnownNat n) => Maybe (Dict (m <= n))
leNat = if natValI @m <= natValI @n
	then Just unsafeRefl
	else Nothing
