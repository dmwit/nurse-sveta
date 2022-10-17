{-# Language DataKinds #-}

module Dr.Mario.Torch (
	Conv2dReLUInitSpec(..),
	Conv2dReLUInit,
	StableSquareConvSpec(..),
	ConstSquareConvSpec, constChansSpec,
	StableSquareConv,
	KnownConfig, KnownAndValidRand,
	) where

import Data.Proxy
import Data.Type.Equality
import GHC.Exts
import GHC.TypeLits
import Torch.Typed hiding (sqrt)

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
		numParameters = fromInteger $
			natVal' (proxy# @kw) * natVal' (proxy# @kh) * natVal' (proxy# @iChans)

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
