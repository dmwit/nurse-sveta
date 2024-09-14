module Nurse.Sveta.Torch (
	Net, Optimizer,
	netSample, netLoadForInference, netLoadForTraining,
	netEvaluation, netLossComponents, netActivations, netGradients, netTrain,
	netSave, netWeights,
	TrainingExample(..), GameDetails, GameStep(..),
	fullReplay, fullReplayV, trainingExamples,
	NetInput(..), NetOutput(..), GroundTruth(..), LossScaling(..),
	netSample', netLoadForInference', netLoadForTraining',
	netEvaluation', netLossComponents', netActivations', netGradients', netTrain',
	)
	where

import Control.Monad
import Data.Aeson
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Dr.Mario.Model
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Torch.CWrapper
import Nurse.Sveta.Torch.Semantics
import Foreign
import Foreign.C

import qualified Data.Vector as V

data GameStep = GameStep
	{ gsRNG :: Lookahead
	, gsPath :: MidPath
	, gsPill :: Pill
	, gsVisits :: HashMap Pill Int
	} deriving (Eq, Ord, Read, Show)

instance ToJSON GameStep where toJSON gs = toJSON (gsRNG gs, gsPath gs, gsPill gs, gsVisits gs)
instance FromJSON GameStep where parseJSON v = parseJSON v <&> \(r, p, m, v) -> GameStep r p m v

-- TODO: could consider varying the cost model, gravity speed, NES vs SNES pill
-- distribution, NES vs SNES (vs other?) pathfinding, and then passing info on
-- which choice was made into the net

type GameDetails = ((Board, Bool, CoarseSpeed), [GameStep], Lookahead, HyperParameters)

foreign import ccall "sample_net" cxx_sample_net :: Ptr CStructure -> Ptr CStructure -> Ptr (Ptr Net) -> Ptr (Ptr Optimizer) -> IO ()
foreign import ccall "load_net" cxx_load_net :: CString -> Ptr CStructure -> Ptr CStructure -> Ptr (Ptr Net) -> Ptr (Ptr Optimizer) -> IO ()
foreign import ccall "&discard_net" cxx_discard_net :: FinalizerPtr Net
foreign import ccall "&discard_optimizer" cxx_discard_optimizer :: FunPtr (Ptr Optimizer -> IO ())
foreign import ccall "save_net" cxx_save_net :: Ptr Net -> Ptr Optimizer -> CString -> IO ()
foreign import ccall "evaluate_net" cxx_evaluate_net :: Ptr Net -> Ptr CEndpoint -> IO (Ptr CEndpoint)
foreign import ccall "loss_components" cxx_loss_components :: Ptr Net -> Ptr CEndpoint -> Ptr CEndpoint -> Ptr CEndpoint -> IO (Ptr CEndpoint)
foreign import ccall "net_activations" cxx_net_activations :: Ptr Net -> Ptr CEndpoint -> IO (Ptr CEndpoint)
foreign import ccall "train_net" cxx_train_net :: Ptr Net -> Ptr Optimizer -> Ptr CEndpoint -> Ptr CEndpoint -> IO CFloat
foreign import ccall "gradients" cxx_gradients :: Ptr Net -> Ptr CEndpoint -> Ptr CEndpoint -> IO (Ptr CEndpoint)
foreign import ccall "net_weights" cxx_net_weights :: Ptr Net -> IO (Ptr CEndpoint)

newtype Net = Net (ForeignPtr Net) deriving newtype CWrapper
newtype Optimizer = Optimizer (ForeignPtr Optimizer) deriving newtype CWrapper

gcNet :: Ptr Net -> IO Net
gcNet ptr = Net <$> newForeignPtr cxx_discard_net ptr

gcOptimizer :: Ptr Optimizer -> IO Optimizer
gcOptimizer ptr = Optimizer <$> newForeignPtr cxx_discard_optimizer ptr

netSample' :: Structure -> Structure -> IO (Net, Optimizer)
netSample' i o = do
	c_i <- cStructure i
	c_o <- cStructure o
	withUnwrapped (c_i, c_o) \(ptr_i, ptr_o) ->
		alloca \ptr_net ->
		alloca \ptr_optim -> do
			cxx_sample_net ptr_i ptr_o ptr_net ptr_optim
			liftM2 (,) (peek ptr_net >>= gcNet) (peek ptr_optim >>= gcOptimizer)

netLoad' :: FilePath -> Structure -> Structure -> Ptr (Ptr Optimizer) -> IO Net
netLoad' path i o ptr_optim = do
	c_i <- cStructure i
	c_o <- cStructure o
	withUnwrapped (WCS path, (c_i, c_o)) \(c_path, (ptr_i, ptr_o)) ->
		alloca \ptr_net -> do
			cxx_load_net c_path ptr_i ptr_o ptr_net ptr_optim
			peek ptr_net >>= gcNet

netLoadForInference' :: FilePath -> Structure -> Structure -> IO Net
netLoadForInference' path i o = netLoad' path i o nullPtr

netLoadForTraining' :: FilePath -> Structure -> Structure -> IO (Net, Optimizer)
netLoadForTraining' path i o = alloca \ptr_optim -> do
	net <- netLoad' path i o ptr_optim
	optim <- peek ptr_optim >>= gcOptimizer
	pure (net, optim)

netEvaluation' :: Net -> Endpoint -> IO Endpoint
netEvaluation' net_ i = do
	c_i <- cEndpoint i
	withUnwrapped (net_, c_i) \(net, ptr_i) ->
		cxx_evaluate_net net ptr_i >>= gcEndpoint >>= hsEndpoint

netLossComponents' :: Net -> Endpoint -> Endpoint -> Endpoint -> IO Endpoint
netLossComponents' net_ scaling_ net_input_ ground_truth_ = do
	c_net_input <- cEndpoint net_input_
	withUnwrapped (net_, c_net_input) \(net, ptr_net_input) -> do
		ptr_net_output <- cxx_evaluate_net net ptr_net_input
		c_scaling <- cEndpoint scaling_
		c_ground_truth <- cEndpoint ground_truth_
		withUnwrapped (c_scaling, c_ground_truth) \(ptr_scaling, ptr_ground_truth) -> do
			ptr_components <- cxx_loss_components net ptr_scaling ptr_net_output ptr_ground_truth
			components <- gcEndpoint ptr_components >>= hsEndpoint
			components <$ gcEndpoint ptr_net_output

netActivations' :: Net -> Endpoint -> IO Endpoint
netActivations' net_ i_ = do
	c_i <- cEndpoint i_
	withUnwrapped (net_, c_i) \(net, ptr_i) ->
		cxx_net_activations net ptr_i >>= gcEndpoint >>= hsEndpoint

netTrain' :: Net -> Optimizer -> Endpoint -> Endpoint -> IO Float
netTrain' net_ optim_ scaling_ batch_ = do
	c_scaling <- cEndpoint scaling_
	c_batch <- cEndpoint batch_
	withUnwrapped (net_, (optim_, (c_scaling, c_batch))) \(net, (optim, (scaling, batch))) ->
		realToFrac <$> cxx_train_net net optim scaling batch

netGradients' :: Net -> Endpoint -> Endpoint -> IO Endpoint
netGradients' net_ scaling_ batch_ = do
	c_scaling <- cEndpoint scaling_
	c_batch <- cEndpoint batch_
	withUnwrapped (net_, (c_scaling, c_batch)) \(net, (scaling, batch)) ->
		cxx_gradients net scaling batch >>= gcEndpoint >>= hsEndpoint

withNetIO :: (Structure -> Structure -> a) -> a
withNetIO f = f (structure @NetInput) (structure @NetOutput)

netSample :: IO (Net, Optimizer)
netSample = withNetIO netSample'

netLoadForInference :: FilePath -> IO Net
netLoadForInference = withNetIO . netLoadForInference'

netLoadForTraining :: FilePath -> IO (Net, Optimizer)
netLoadForTraining = withNetIO . netLoadForTraining'

netSave :: Net -> Optimizer -> FilePath -> IO ()
netSave net_ optim_ path_ = withUnwrapped (WCS path_, (net_, optim_)) \(path, (net, optim)) ->
	cxx_save_net net optim path

netEvaluation :: Net -> Vector NetInput -> IO (Vector NetOutput)
netEvaluation net is = fromEndpoint <$> netEvaluation' net (toEndpoint is)

netActivations :: Net -> Vector NetInput -> IO Endpoint
netActivations net is = netActivations' net (toEndpoint is)

-- TODO: could do a slight optimization here (in netLossComponents and
-- netTrain) where we serialize LossScaling just once and pass around a
-- CEndpoint thereafter, but it would take a bit of work, including mildly
-- invasive changes to callers
netLossComponents :: Net -> LossScaling -> Vector TrainingExample -> IO [([String], Float)]
netLossComponents net scaling batch = flatten [] <$> netLossComponents' net (lsEndpoint scaling) (toEndpoint (teInput <$> batch)) (toEndpoint (teTruth <$> batch)) where
	flatten prefix = \case
		EFullTensor [] vals -> [(reverse prefix, realToFrac (the (vals ! 0)))]
		EFullTensor{} -> error "flattening complicated tensor loss scalings is not (yet) implemented"
		EMaskedTensor{} -> error "loss_components returned a masked tensor; this is almost certainly a bug in loss_components"
		EVector gc es -> do
			(i, e) <- zip [0..] es
			flatten (show i:prefix) e
		EDictionary dict -> do
			(k, v) <- dict
			flatten (k:prefix) v

netTrain :: Net -> Optimizer -> LossScaling -> Vector TrainingExample -> IO Float
netTrain net optim scaling batch = netTrain' net optim (lsEndpoint scaling) (toEndpoint batch)

netWeights :: Net -> IO Endpoint
netWeights net_ = withUnwrapped net_ (cxx_net_weights >=> gcEndpoint >=> hsEndpoint)

netGradients :: Net -> LossScaling -> Vector TrainingExample -> IO Endpoint
netGradients net scaling batch = netGradients' net (lsEndpoint scaling) (toEndpoint batch)

fullReplay :: (GameStateSeed a, Show a) => a -> [GameStep] -> Lookahead -> (IGameState -> s) -> (GameStep -> IGameState -> ClearResults -> s -> s) -> IO s
fullReplay seed steps0 lk fDone fStep = initialState seed >>= go steps0 where
	go [] gs = do
		playRNG gs lk
		igs <- freezeGameState gs
		pure (fDone igs)
	go (step:steps) gs = do
		playRNG gs (gsRNG step)
		igs <- freezeGameState gs
		playMove gs (gsPath step) (gsPill step) >>= \case
			Nothing -> fail $ ""
				++ "illegal move in game record; seed=" ++ show seed
				++ ", steps=" ++ show steps0
				++ ", steps remaining=" ++ show (length (step:steps))
			Just res -> fStep step igs res <$> go steps gs

fullReplayV :: (GameStateSeed a, Show a) => a -> [GameStep] -> Lookahead -> (IGameState -> s) -> (GameStep -> IGameState -> ClearResults -> s -> (s, v)) -> IO (Vector v)
fullReplayV seed steps lk fDone fStep = V.fromList . snd <$> fullReplay seed steps lk
	(\igs -> (fDone igs, []))
	(\step igs res (s, vs) -> (:vs) <$> fStep step igs res s)

trainingExamples :: GameDetails -> IO (Vector TrainingExample)
trainingExamples (seed, steps, lk, hp) = fullReplayV seed steps lk (\igs -> (ihpFinalReward hp igs, iFramesPassed igs)) \step igs res (value, frames) -> let
	value' = hpImmediateReward hp res + hpDiscount hp (frames - frames') value
	frames' = iFramesPassed igs
	scaling = recip . fromIntegral . max 1 . sum $ gsVisits step
	te = TrainingExample
		{ teInput = NetInput
			{ niBoard = iBoard igs
			, niFrames = frames'
			, niOriginalVirusCount = iOriginalVirusCount igs
			}
		, teTruth = GroundTruth
			{ gtPriors = gsVisits step <&> \n -> scaling * fromIntegral n
			, gtValuation = value'
			}
		}
	in ((value', frames'), te)
