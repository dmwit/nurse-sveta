module Nurse.Sveta.Torch (
	NextNet, Optimizer,
	nextNetSample, nextNetLoadForInference, nextNetLoadForTraining,
	nextNetEvaluation, nextNetLossComponents, nextNetTrain,
	nextNetSave, nextNetWeights,
	NextTrainingExample(..), GameDetails, GameStep(..),
	nextTrainingExamples,
	NextNetInput(..), NextNetOutput(..), NextGroundTruth(..), NextLossScaling(..),
	nextNetSample', nextNetLoadForInference', nextNetLoadForTraining',
	nextNetEvaluation', nextNetLossComponents', nextNetTrain',
	)
	where

import Control.Monad
import Data.Aeson
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Vector (Vector)
import Dr.Mario.Model
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Torch.CWrapper
import Nurse.Sveta.Torch.Semantics
import Foreign
import Foreign.C

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

data GameStep = GameStep
	{ gsMove :: Move
	, gsRoot :: Statistics
	, gsChildren :: HashMap Move Statistics
	} deriving (Eq, Ord, Read, Show)

instance ToJSON GameStep where toJSON gs = toJSON (gsMove gs, gsRoot gs, HM.toList (gsChildren gs))
instance FromJSON GameStep where parseJSON v = parseJSON v <&> \(m, r, c) -> GameStep m r (HM.fromList c)

-- TODO: could consider varying the cost model, gravity speed, NES vs SNES pill
-- distribution, NES vs SNES (vs other?) pathfinding, and then passing info on
-- which choice was made into the net

type GameDetails = ((Board, Bool, CoarseSpeed), [GameStep])

foreign import ccall "next_sample_net" cxx_next_sample_net :: Ptr CStructure -> Ptr CStructure -> Ptr (Ptr NextNet) -> Ptr (Ptr Optimizer) -> IO ()
foreign import ccall "next_load_net" cxx_next_load_net :: CString -> Ptr CStructure -> Ptr CStructure -> Ptr (Ptr NextNet) -> Ptr (Ptr Optimizer) -> IO ()
foreign import ccall "&next_discard_net" cxx_next_discard_net :: FinalizerPtr NextNet
foreign import ccall "&discard_optimizer" cxx_discard_optimizer :: FunPtr (Ptr Optimizer -> IO ())
foreign import ccall "next_save_net" cxx_next_save_net :: Ptr NextNet -> Ptr Optimizer -> CString -> IO ()
foreign import ccall "next_evaluate_net" cxx_next_evaluate_net :: Ptr NextNet -> Ptr CEndpoint -> IO (Ptr CEndpoint)
foreign import ccall "next_loss_components" cxx_next_loss_components :: Ptr NextNet -> Ptr CEndpoint -> Ptr CEndpoint -> Ptr CEndpoint -> IO (Ptr CEndpoint)
foreign import ccall "next_train_net" cxx_next_train_net :: Ptr NextNet -> Ptr Optimizer -> Ptr CEndpoint -> Ptr CEndpoint -> IO CFloat
foreign import ccall "next_net_weights" cxx_next_net_weights :: Ptr NextNet -> IO (Ptr CEndpoint)

newtype NextNet = NextNet (ForeignPtr NextNet) deriving newtype CWrapper
newtype Optimizer = Optimizer (ForeignPtr Optimizer) deriving newtype CWrapper

gcNextNet :: Ptr NextNet -> IO NextNet
gcNextNet ptr = NextNet <$> newForeignPtr cxx_next_discard_net ptr

gcOptimizer :: Ptr Optimizer -> IO Optimizer
gcOptimizer ptr = Optimizer <$> newForeignPtr cxx_discard_optimizer ptr

nextNetSample' :: Structure -> Structure -> IO (NextNet, Optimizer)
nextNetSample' i o = do
	c_i <- cStructure i
	c_o <- cStructure o
	withUnwrapped (c_i, c_o) \(ptr_i, ptr_o) ->
		alloca \ptr_net ->
		alloca \ptr_optim -> do
			cxx_next_sample_net ptr_i ptr_o ptr_net ptr_optim
			-- TODO: rename gcOptimizer to gcOptimizer
			liftM2 (,) (peek ptr_net >>= gcNextNet) (peek ptr_optim >>= gcOptimizer)

nextNetLoad' :: FilePath -> Structure -> Structure -> Ptr (Ptr Optimizer) -> IO NextNet
nextNetLoad' path i o ptr_optim = do
	c_i <- cStructure i
	c_o <- cStructure o
	withUnwrapped (WCS path, (c_i, c_o)) \(c_path, (ptr_i, ptr_o)) ->
		alloca \ptr_net -> do
			cxx_next_load_net c_path ptr_i ptr_o ptr_net ptr_optim
			peek ptr_net >>= gcNextNet

nextNetLoadForInference' :: FilePath -> Structure -> Structure -> IO NextNet
nextNetLoadForInference' path i o = nextNetLoad' path i o nullPtr

nextNetLoadForTraining' :: FilePath -> Structure -> Structure -> IO (NextNet, Optimizer)
nextNetLoadForTraining' path i o = alloca \ptr_optim -> do
	net <- nextNetLoad' path i o ptr_optim
	optim <- peek ptr_optim >>= gcOptimizer
	pure (net, optim)

nextNetEvaluation' :: NextNet -> Endpoint -> IO Endpoint
nextNetEvaluation' net_ i = do
	c_i <- cEndpoint i
	withUnwrapped (net_, c_i) \(net, ptr_i) ->
		cxx_next_evaluate_net net ptr_i >>= gcEndpoint >>= hsEndpoint

nextNetLossComponents' :: NextNet -> Endpoint -> Endpoint -> Endpoint -> IO Endpoint
nextNetLossComponents' net_ scaling_ net_input_ ground_truth_ = do
	c_net_input <- cEndpoint net_input_
	withUnwrapped (net_, c_net_input) \(net, ptr_net_input) -> do
		ptr_net_output <- cxx_next_evaluate_net net ptr_net_input
		c_scaling <- cEndpoint scaling_
		c_ground_truth <- cEndpoint ground_truth_
		withUnwrapped (c_scaling, c_ground_truth) \(ptr_scaling, ptr_ground_truth) -> do
			ptr_components <- cxx_next_loss_components net ptr_scaling ptr_net_output ptr_ground_truth
			components <- gcEndpoint ptr_components >>= hsEndpoint
			components <$ gcEndpoint ptr_net_output

nextNetTrain' :: NextNet -> Optimizer -> Endpoint -> Endpoint -> IO Float
nextNetTrain' net_ optim_ scaling_ batch_ = do
	c_scaling <- cEndpoint scaling_
	c_batch <- cEndpoint batch_
	withUnwrapped (net_, (optim_, (c_scaling, c_batch))) \(net, (optim, (scaling, batch))) ->
		realToFrac <$> cxx_next_train_net net optim scaling batch

withNextNetIO :: (Structure -> Structure -> a) -> a
withNextNetIO f = f (structure @NextNetInput) (structure @NextNetOutput)

nextNetSample :: IO (NextNet, Optimizer)
nextNetSample = withNextNetIO nextNetSample'

nextNetLoadForInference :: FilePath -> IO NextNet
nextNetLoadForInference = withNextNetIO . nextNetLoadForInference'

nextNetLoadForTraining :: FilePath -> IO (NextNet, Optimizer)
nextNetLoadForTraining = withNextNetIO . nextNetLoadForTraining'

nextNetSave :: NextNet -> Optimizer -> FilePath -> IO ()
nextNetSave net_ optim_ path_ = withUnwrapped (WCS path_, (net_, optim_)) \(path, (net, optim)) ->
	cxx_next_save_net net optim path

nextNetEvaluation :: NextNet -> Vector NextNetInput -> IO (Vector NextNetOutput)
nextNetEvaluation net is = fromEndpoint <$> nextNetEvaluation' net (toEndpoint is)

-- TODO: could do a slight optimization here (in nextNetLossComponents and
-- nextNetTrain) where we serialize NextLossScaling just once and pass around a
-- CEndpoint thereafter, but it would take a bit of work, including mildly
-- invasive changes to callers
nextNetLossComponents :: NextNet -> NextLossScaling -> Vector NextTrainingExample -> IO [([String], Float)]
nextNetLossComponents net scaling batch = flatten [] <$> nextNetLossComponents' net (lsEndpoint scaling) (toEndpoint (teInput <$> batch)) (toEndpoint (teTruth <$> batch)) where
	flatten prefix = \case
		EFullTensor [] vals -> [(reverse prefix, realToFrac (the vals))]
		EFullTensor{} -> error "flattening complicated tensor loss scalings is not (yet) implemented"
		EMaskedTensor{} -> error "next_loss_components returned a masked tensor; this is almost certainly a bug in next_loss_components"
		EVector gc es -> do
			(i, e) <- zip [0..] es
			flatten (show i:prefix) e
		EDictionary dict -> do
			(k, v) <- dict
			flatten (k:prefix) v

nextNetTrain :: NextNet -> Optimizer -> NextLossScaling -> Vector NextTrainingExample -> IO Float
nextNetTrain net optim scaling batch = nextNetTrain' net optim (lsEndpoint scaling) (toEndpoint batch)

nextNetWeights :: NextNet -> IO Endpoint
nextNetWeights net_ = withUnwrapped net_ (cxx_next_net_weights >=> gcEndpoint >=> hsEndpoint)

nextTrainingExamples :: GameDetails -> IO (Vector NextTrainingExample)
nextTrainingExamples (b0, steps) = do
	currentState <- initialState b0
	-- gonna play through the whole game real quick to evaluate the final
	-- board, so make a fresh clone of the state
	tmpState <- initialState b0
	valuation <- mapM (dmPlay tmpState . gsMove) steps >> evaluateFinalState tmpState
	let numPlacements = length [() | GameStep { gsMove = Placement{} } <- steps]
	    loop (gs:gss) = case gsMove gs of
	    	-- subtlety: dmPlay records the lookbehind
	    	RNG{} -> dmPlay currentState (gsMove gs) >> loop gss
	    	Placement{} -> do
	    		frames <- readIORef (framesPassed currentState)
	    		board <- mfreeze (board currentState)
	    		lookbehind <- readIORef (lookbehind currentState)

	    		-- TODO: why is the root's visit count always one bigger than the sum of the children's visit counts?
	    		let scaling = recip . max 1 . visitCount $ gsRoot gs
	    		    priors = HM.fromList [(pill, scaling * visitCount stats) | (Placement _ pill, stats) <- HM.toList (gsChildren gs)]
	    		    te = NextTrainingExample
	    		    	{ teInput = NextNetInput
	    		    		{ niBoard = board
	    		    		, niFrames = frames
	    		    		, niOriginalVirusCount = originalVirusCount currentState
	    		    		}
	    		    	, teTruth = NextGroundTruth
	    		    		{ gtPriors = priors
	    		    		, gtLookahead = lookbehind
	    		    		, gtValuation = valuation
	    		    		}
	    		    	}
	    		(te, gss) <$ dmPlay currentState (gsMove gs)

	V.unfoldrExactNM numPlacements loop steps
