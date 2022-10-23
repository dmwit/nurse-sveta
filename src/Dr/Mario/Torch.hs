{-# Language DataKinds #-}

module Dr.Mario.Torch (
	NurseSvetaNetDef, netDefForward,
	NurseSvetaNetSpec(..), NurseSvetaNet, netForward,
	NetInput(..),
	NetOutput(..),
	ResidualSpec(..), Residual, residualForward,
	StableSquareConvSpec(..), ConstSquareConvSpec, constChansSpec,
	StableSquareConv, ConstSquareConv,
	Conv2dReLUInitSpec(..), Conv2dReLUInit,
	BatchNorm, bnNew, bnForward,
	unsafeAsTensor,
	KnownConfig, KnownAndValidRand, BatchNormDTypeIsValid,
	) where

import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Proxy
import Data.Type.Equality
import Dr.Mario.Model
import GHC.Exts
import GHC.TypeLits
import Torch.Typed hiding (sqrt)

import qualified Data.Vector as VU
import qualified Data.Vector.Sized as VS
import qualified Torch.Typed as T
import qualified Torch as TU

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
newtype StableSquareConvSpec (iChans :: Nat) (oChans :: Nat) (p :: Nat) (dtype :: DType) (dev :: (DeviceType, Nat)) = StableSquareConvSpec { stableSquareLeakage :: Double }
	deriving (Eq, Ord, Read, Show)

constChansSpec :: Double -> ConstSquareConvSpec chans k dtype dev
constChansSpec = StableSquareConvSpec

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
	sample (StableSquareConvSpec n) = StableSquareConv <$> sample (Conv2dReLUInitSpec n)

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

bnNew :: KnownConfig '[c] dtype dev =>
	-- | momentum (the sensible kind, like all the other NN layers -- e.g. 0.9ish is sensible)
	Double ->
	-- | epsilon
	Double ->
	IO (BatchNorm c dtype dev)
bnNew momentum epsilon = pure BatchNorm
	<*> makeIndependent ones
	<*> makeIndependent zeros
	<*> newIORef zeros
	<*> newIORef zeros
	<*> pure (expand False (full @'[] momentum))
	<*> pure (expand False (full @'[] epsilon))

type BatchNormDTypeIsValid dev dtype =
	( KnownDevice dev
	, MeanDTypeValidation dev dtype
	, SumDTypeIsValid dev dtype
	, SumDType dtype ~ dtype
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
	( KnownDevice dev
	, All KnownNat [n, c]
	, AllDimsPositive [n, c]
	, MeanDTypeValidation dev dtype
	, SumDTypeIsValid dev dtype, SumDType dtype ~ dtype
	, BasicArithmeticDTypeIsValid dev dtype
	, StandardFloatingPointDTypeValidation dev dtype
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

newtype ResidualSpec chans p dtype dev = ResidualSpec { residualSpecLeakage :: Double }
	deriving (Eq, Ord, Read, Show)

data Residual chans p dtype dev = Residual
	{ resLeakage :: Double
	, resConv0, resConv1 :: ConstSquareConv chans p dtype dev
	, resNorm0, resNorm1 :: BatchNorm chans dtype dev
	}

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
data NetInput = NetInput
	{ niBoard :: IOBoard
	, niLookaheadL :: Color
	, niLookaheadR :: Color
	}

-- TODO: ideas for other outputs: what position will the next occurrence of
-- each kind of pill go? will each position be empty at end of game? maybe
-- something about what order the viruses are cleared in, or what frame it
-- happens on?
data NetOutput = NetOutput
	{ noPriors :: HashMap PillContent (VU.Vector (VU.Vector Double))
	, noValuation :: Double
	, noVirusClears :: Double
	, noPills :: Double
	, noFrames :: Double
	}

newtype NurseSvetaNetSpec depth filters p dev = NurseSvetaNetSpec { nsLeakage :: Double }
	deriving (Eq, Ord, Read, Show)

type PerCell n = Numel [n,8,16]

data NurseSvetaNet depth filters p dev = NurseSvetaNet
	{ netBoardConvolution :: StableSquareConv 13 filters p 'Double dev
	, netInputLinear :: Linear 6 (PerCell filters) 'Double dev
	, netInputNormalization :: BatchNorm filters 'Double dev
	, netResiduals :: VS.Vector depth (Residual filters p 'Double dev)
	, netOutputLinear :: Linear (PerCell filters) 4 'Double dev
	, netPriorsConvolution :: StableSquareConv filters 4 p 'Double dev
	}

netForward :: forall n depth filters p dev k.
	( k ~ (2*p+1)
	, BatchNormDTypeIsValid dev 'Double
	, All KnownNat [filters, p, k, n, PerCell n, PerCell filters]
	, AllDimsPositive [PerCell n, filters]
	, ConvSideCheck  8 k 1 p  8
	, ConvSideCheck 16 k 1 p 16
	) =>
	NurseSvetaNet depth filters p dev ->
	Bool ->
	Tensor dev 'Double [n, 13, 8, 16] ->
	Tensor dev 'Double [n, 6] ->
	IO (Tensor dev 'Double [n, 4, 8, 16], Tensor dev 'Double [n, 4])
netForward net training board scalars = do
	let a = forward (netBoardConvolution net) board
	    b = forward (netInputLinear net) scalars
	    c = a + reshape b
	d <- bnForward (netInputNormalization net) training c
	e <- VS.foldM (\tensor layer -> residualForward layer training tensor) d (netResiduals net)
	let f = forward (netPriorsConvolution net) e
	    g = forward (netOutputLinear net) (reshape @[n, PerCell filters] e)
	pure (f, g)

type NurseSvetaNetDef = NurseSvetaNet 30 64 1

netDefForward ::
	( BatchNormDTypeIsValid dev 'Double
	, KnownNat n, KnownNat (PerCell n)
	, 1 <= PerCell n
	) =>
	NurseSvetaNetDef dev ->
	Bool ->
	Tensor dev 'Double [n, 13, 8, 16] ->
	Tensor dev 'Double [n, 6] ->
	IO (Tensor dev 'Double [n, 4, 8, 16], Tensor dev 'Double [n, 4])
netDefForward = netForward
