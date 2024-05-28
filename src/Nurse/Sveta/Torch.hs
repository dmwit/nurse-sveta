module Nurse.Sveta.Torch (
	NextNet,
	nextNetSample, nextNetEvaluation, nextNetIntrospect,
	NextTrainingExample(..), -- TODO: move GameDetails here
	nextTrainingExamples,
	NextNetInput(..), NextNetOutput(..), NextGroundTruth(..),
	nextNetSample', nextNetEvaluation',

	Net, netSample, netEvaluation, netTrain, netDetailedLoss, netIntrospect,
	netSave, netLoadForInference, netLoadForTraining,
	HSTensor(..), Prediction(..), NetIntrospection(..),
	LossType(..), describeLossType,
	Optimizer, newOptimizer,
	Batch, batchLoad,
	GameStep(..), GameDetails, SaveTensorsSummary(..), saveTensors,
	)
	where

import Control.Monad
import Control.Monad.Primitive
import Data.Aeson
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap.Strict (IntMap)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Traversable
import Data.Universe
import Data.Vector (Vector)
import Dr.Mario.Model
import Dr.Mario.Model.Internal
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Torch.CWrapper
import Nurse.Sveta.Torch.Endpoint
import Nurse.Sveta.Util
import Foreign
import Foreign.C
import GHC.Generics
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MV

-- I played with marking these as unsafe. It really hurt performance a lot,
-- plus it made the UI unbearably choppy.
foreign import ccall "sample_net" cxx_sample_net :: Bool -> IO (Ptr Net)
foreign import ccall "&discard_net" cxx_discard_net :: FunPtr (Ptr Net -> IO ())
foreign import ccall "evaluate_net" cxx_evaluate_net :: Ptr Net -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CChar -> Ptr CChar -> Ptr CFloat -> IO ()
foreign import ccall "introspect_net" cxx_introspect_net :: Ptr Net -> Ptr Batch -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall "save_example" cxx_save_example :: CString -> Ptr CChar -> Ptr CFloat -> CFloat -> CUChar -> Ptr CChar -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CChar -> Ptr CChar -> Ptr CFloat -> IO ()
foreign import ccall "load_batch" cxx_load_batch :: Ptr CString -> CInt -> IO (Ptr Batch)
foreign import ccall "batch_size" cxx_batch_size :: Ptr Batch -> IO CInt
foreign import ccall "&discard_batch" cxx_discard_batch :: FunPtr (Ptr Batch -> IO ())
foreign import ccall "train_net" cxx_train_net :: Ptr Net -> Ptr Optimizer -> Ptr Batch -> Ptr CFloat -> IO CFloat
foreign import ccall "detailed_loss" cxx_detailed_loss :: Ptr Net -> Ptr CFloat -> Ptr Batch -> Ptr CFloat -> IO ()
foreign import ccall "save_net" cxx_save_net :: Ptr Net -> Ptr Optimizer -> CString -> IO ()
foreign import ccall "load_net" cxx_load_net :: CString -> Ptr (Ptr Net) -> Ptr (Ptr Optimizer) -> IO ()
foreign import ccall "connect_optimizer" cxx_connect_optimizer :: Ptr Net -> IO (Ptr Optimizer)
foreign import ccall "&discard_optimizer" cxx_discard_optimizer :: FunPtr (Ptr Optimizer -> IO ())

newtype Net = Net (ForeignPtr Net) deriving newtype CWrapper
newtype Batch = Batch (ForeignPtr Batch) deriving newtype CWrapper
newtype Optimizer = Optimizer (ForeignPtr Optimizer) deriving newtype CWrapper

mkNet :: Ptr Net -> IO Net
mkNet ptr = Net <$> newForeignPtr cxx_discard_net ptr

mkBatch :: Ptr Batch -> IO Batch
mkBatch ptr = Batch <$> newForeignPtr cxx_discard_batch ptr

mkOptimizer :: Ptr Optimizer -> IO Optimizer
mkOptimizer ptr = Optimizer <$> newForeignPtr cxx_discard_optimizer ptr

netSample :: Bool -> IO Net
netSample = cxx_sample_net >=> mkNet

batchLoad :: [FilePath] -> IO Batch
batchLoad paths = withUnwrapped (WCS <$> paths) $ flip withArrayLen $ \n cpaths ->
	cxx_load_batch cpaths (fromIntegral n) >>= mkBatch

newOptimizer :: Net -> IO Optimizer
newOptimizer net = withUnwrapped net $ cxx_connect_optimizer >=> mkOptimizer

netSave :: Net -> Optimizer -> FilePath -> IO ()
netSave net_ optim_ fp = withUnwrapped (net_, (optim_, WCS fp)) $ \(net, (optim, cfp)) ->
	cxx_save_net net optim cfp

netLoadForInference :: FilePath -> IO Net
netLoadForInference path = withUnwrapped (WCS path) $ \cpath -> do
	netPtr <- malloc
	cxx_load_net cpath netPtr nullPtr
	peek netPtr >>= mkNet

netLoadForTraining :: FilePath -> IO (Net, Optimizer)
netLoadForTraining path = withUnwrapped (WCS path) $ \cpath -> do
	netPtr <- malloc
	optimPtr <- malloc
	cxx_load_net cpath netPtr optimPtr
	liftM2 (,) (peek netPtr >>= mkNet) (peek optimPtr >>= mkOptimizer)

netDetailedLoss :: Net -> Batch -> (LossType -> Float) -> IO [(LossType, Float)]
netDetailedLoss net_ batch_ scaling_ = withUnwrapped (net_, batch_) $ \(net, batch) ->
	allocaArray lossTypes $ \out ->
		withLossScaling scaling_ $ \scaling -> do
			cxx_detailed_loss net out batch scaling
			zipWith (\ty w -> (ty, realToFrac w)) [minBound..] <$> peekArray lossTypes out

renderLookahead :: Ptr CChar -> Lookahead -> IO ()
renderLookahead lookahead (Lookahead l r) = do
	pokeElemOff lookahead (toIndex l                    ) 1
	pokeElemOff lookahead (toIndex r + indexCount @Color) 1

-- TODO: do we really want to duplicate this code? perhaps we should just mfreeze >=> renderBoard
renderIOBoard :: Ptr CChar -> IOBoard -> IO ()
renderIOBoard board b = do
	unless (mwidth b == boardWidth && mheight b == boardHeight) . fail $
		"expected all boards to have size " ++ show boardWidth ++ "x" ++ show boardHeight ++ ", but saw a board with size " ++ show (mwidth b) ++ "x" ++ show (mheight b)
	-- IOBoard stores cells in a 1D array with the y coordinate varying
	-- fastest, just like the tensor we want to make. This means we get to
	-- reuse the index into that array as an index into our array. Nice.
	MV.iforM_ (mcells b) $ \j -> \case
		Empty -> pure ()
		Occupied c s -> do
			pokeElemOff board (shiftL (                    toIndex c) logCellCount + j) 1
			pokeElemOff board (shiftL (indexCount @Color + toIndex s) logCellCount + j) 1

renderBoard :: Ptr CChar -> Board -> IO ()
renderBoard board b = do
	unless (width b == boardWidth && height b == boardHeight) . fail $
		"expected all boards to have size " ++ show boardWidth ++ "x" ++ show boardHeight ++ ", but saw a board with size " ++ show (width b) ++ "x" ++ show (height b)
	for_ [0..boardWidth-1] $ \x ->
		for_ [0..boardHeight-1] $ \y ->
			let pos = Position x y
			    i = iPos pos
			in case unsafeGet b pos of
			Empty -> pure ()
			Occupied c s -> do
				pokeElemOff board (shiftL (                    toIndex c) logCellCount + i) 1
				pokeElemOff board (shiftL (indexCount @Color + toIndex s) logCellCount + i) 1

renderScalars :: Ptr CFloat -> GameState -> IO [Float]
renderScalars scalars gs = do
	frames_ <- readIORef (framesPassed gs)
	let [frames, viruses] = fromIntegral <$> [frames_, originalVirusCount gs]
	pokeArray scalars [frames, safeLog frames, sqrt frames, viruses, safeLog viruses, 1/sqrt viruses]
	pure [realToFrac frames, realToFrac viruses]

safeLog :: CFloat -> CFloat
safeLog = log . max (exp (-1))

render :: Traversable t => t (Int, GameState) -> IO (Ptr CChar, Ptr CChar, Ptr CFloat)
render igs = do
	boards <- mallocZeroArray (n * boardSize)
	lookaheads <- mallocZeroArray (n * lookaheadSize)
	scalars <- mallocArray (n * numScalars)
	for_ igs $ \(i, gs) -> do
		renderIOBoard   (plusArray boards     (i*    boardSize)) (board gs)
		renderLookahead (plusArray lookaheads (i*lookaheadSize)) =<< readIORef (lookbehind gs)
		renderScalars   (plusArray scalars    (i*   numScalars)) gs
	pure (boards, lookaheads, scalars)
	where
	n = length igs

-- It looks like there's a lot of realToFrac calls in this code, but it doesn't
-- matter, because rewrite rules throw them away.
parseForEvaluation :: Int -> GameState -> ForeignPtr CFloat -> ForeignPtr CFloat -> IO DetailedEvaluation
parseForEvaluation i gs priors_ valuation_ = withUnwrapped (priors_, valuation_) $ \(priors, valuation) -> do
	lk <- readIORef (lookbehind gs)
	v <- peekElemOff valuation i
	p <- forZipWithM [0..rotations-1] (iterate (`rotateContent` Clockwise) (launchContent lk)) $ \numRots pc -> do
		let iNumRots = iPriors + shiftL numRots logCellCount
		v <- V.generateM boardWidth $ \x -> let ix = iNumRots + shiftL x logBoardHeight in
			V.generateM boardHeight $ \y -> let iy = ix + y in
				realToFrac <$> peekElemOff priors iy
		pure (pc, v)

	pure (realToFrac v, HM.fromListWith (V.zipWith (V.zipWith (+))) p)
	where
	iPriors = shiftL i logNumPriors

-- TODO: I wonder if we could improve throughput by pushing the rendering into the generation thread, like we did with the parsing
netEvaluation :: Traversable t => Net -> t GameState -> IO (t DetailedEvaluation)
netEvaluation net_ gss = do
	[priors_, valuation_] <- mallocForeignPtrArrays [shiftL n logNumPriors, n]
	-- TODO: can we avoid a ton of allocation here by pooling allocations of each size -- or even just the largest size, per Net, say?
	(boards, lookaheads, scalars) <- render igs

	withUnwrapped (net_, (priors_, valuation_)) $ \(net, (priors, valuation)) ->
		cxx_evaluate_net net (fromIntegral n) priors valuation boards lookaheads scalars
	result <- for igs $ \(i, gs) ->
			-- unsafeInterleaveIO: turns out parseForEvaluation is a bottleneck
			-- to making foreign calls, so spread its work across multiple
			-- threads by having the consuming thread perform it rather than
			-- this one.
			--
			-- It is... a little bit subtle that this is safe, due to reading
			-- IORefs in parseForEvaluation. I'm pretty sure it is with the way
			-- we are currently searching trees -- namely, cloning the root
			-- GameState and modifying the clone just long enough to reach a
			-- position to evaluate, then throwing it away. All the
			-- modification will be done by the time we call netEvaluation,
			-- then it will be thrown away, hence the IORefs won't ever be
			-- modified again.
			unsafeInterleaveIO (parseForEvaluation i gs priors_ valuation_)

	free boards
	free lookaheads
	free scalars

	pure result
	where
	n = length gss
	igs = enumerate gss

-- TODO: There sure are a lot of really similar types in here, e.g.
-- DetailedEvaluation, Preview, Prediction, HSTensor, not to mention Ptr
-- CFloat -> Ptr CFloat -> Ptr CFloat -> .... It would probably be good to
-- take a step back and think about abstraction boundaries and APIs a bit, then
-- refactor this stuff to be a bit more coherent/consistent.
data NetIntrospection = NetIntrospection
	{ niPriors :: Vector (Vector (Vector Float)) -- ^ clockwise rotations, x, y
	, niValuation :: Float
	, niFallTime :: Float
	, niOccupied :: Vector (Vector Float)
	, niVirusKills :: Vector (Vector Float)
	, niPlacements :: HashMap PillContent (Vector (Vector Float))
	, niClearLocation :: Vector (Vector Float)
	, niClearPill :: HashMap PillContent Float
	} deriving (Eq, Ord, Read, Show)

parseForIntrospection :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO NetIntrospection
parseForIntrospection priors valuation fallTime occupied virusKills wishlist clearLocation clearPill = pure NetIntrospection
	<*> V.generateM rotations (\rot -> boardFullOfFloats (plusArray priors (shiftL rot logCellCount)))
	<*> peekD valuation
	<*> peekD fallTime
	<*> boardFullOfFloats occupied
	<*> boardFullOfFloats virusKills
	<*> for pillContentMap (\i -> boardFullOfFloats (plusArray wishlist (shiftL i logCellCount)))
	<*> boardFullOfFloats clearLocation
	<*> for pillContentMap (peekArrayD clearPill)
	where
	peekArrayD :: Ptr CFloat -> Int -> IO Float
	peekArrayD arr = fmap realToFrac . peek . plusArray arr

	peekD :: Ptr CFloat -> IO Float
	peekD = fmap realToFrac . peek

	boardFullOfFloats :: Ptr CFloat -> IO (Vector (Vector Float))
	boardFullOfFloats arr =
		V.generateM boardWidth $ \x -> let ix = shiftL x logBoardHeight in
		V.generateM boardHeight $ \iy ->
		peekArrayD arr (ix+iy)

	pillContentMap :: HashMap PillContent Int
	pillContentMap = HM.fromList [(fromIndex i, i) | i <- [0..indexCount @PillContent-1]]

netIntrospect :: Net -> Batch -> IO [NetIntrospection]
netIntrospect net_ batch_ = do
	n <- fromIntegral <$> withUnwrapped batch_ cxx_batch_size
	let outputSizes = indices n
	-- don't need to do the ForeignPtr thing here (like we do in netEvaluation) because we're not using unsafeInterleaveIO
	out <- mallocArray (sum outputSizes)
	let [priors, valuation, fallTime, occupied, virusKills, wishlist, clearLocation, clearPill, _onePastTheEnd] = scanl plusArray out outputSizes

	withUnwrapped (net_, batch_) $ \(net, batch) ->
		cxx_introspect_net net batch priors valuation fallTime occupied virusKills wishlist clearLocation clearPill
	result <- for [0..n-1] $ \i -> do
		let [priors', valuation', fallTime', occupied', virusKills', wishlist', clearLocation', clearPill'] = zipWith plusArray
		    	[priors, valuation, fallTime, occupied, virusKills, wishlist, clearLocation, clearPill]
		    	(indices i)
		parseForIntrospection priors' valuation' fallTime' occupied' virusKills' wishlist' clearLocation' clearPill'

	free out

	pure result
	where
	indices i = [shiftL i logNumPriors, i, i, iCells, iCells, shiftL iPC logCellCount, iCells, iPC] where
		iCells = shiftL i logCellCount
		iPC = i*indexCount @PillContent

data GameStep = GameStep
	{ gsMove :: Move
	, gsRoot :: Statistics
	, gsChildren :: HashMap Move Statistics
	} deriving (Eq, Ord, Read, Show)

instance ToJSON GameStep where toJSON gs = toJSON (gsMove gs, gsRoot gs, HM.toList (gsChildren gs))
instance FromJSON GameStep where parseJSON v = parseJSON v <&> \(m, r, c) -> GameStep m r (HM.fromList c)

-- Ints are how many pills were used when the thing happened
data Preview = Preview
	{ pVirusKills :: Map Position Int
	, pLastVirusKill :: Int
	, pPlacements :: HashMap Pill [Int]
	, pClearLocation :: Map Position [Int]
	, pClearPill :: HashMap PillContent [Int]
	, pFallTime :: IntMap Int
	, pFinalBoard :: Board -- ask the net to predict whether spaces are occupied at game end or not
	, pTotalFrames :: Int
	, pFinalValuation :: Float
	} deriving (Eq, Ord, Read, Show)

summarize :: GameState -> [Move] -> IO Preview
summarize gs [] = do
	b <- mfreeze (board gs)
	fp <- readIORef (framesPassed gs)
	v <- evaluateFinalState gs
	pure Preview
		{ pVirusKills = M.empty
		, pLastVirusKill = -1
		, pPlacements = HM.empty
		, pClearLocation = M.empty
		, pClearPill = HM.empty
		, pFallTime = IM.empty
		, pFinalBoard = b
		, pTotalFrames = fp
		, pFinalValuation = v
		}
summarize gs (RNG _ _:ms) = summarize gs ms
summarize gs (Placement path pill:ms) = mplaceDetails (board gs) pill >>= \case
	Nothing -> summarize gs [] -- uhhh... recorded a game with an illegal move, I guess?
	Just cr -> do
		pu <- readIORef (pillsUsed gs)
		writeIORef (pillsUsed gs) $! succ pu
		let summary = summarizeClearResults cr
		    allClears = allClearsFor cr
		    fallTime = fallTimeFor cr
		modifyIORef' (virusesKilled gs) (clears summary +)
		modifyIORef' (framesPassed gs) (approximateCostModel path pill summary +)
		pre <- summarize gs ms
		pure pre
			{ pVirusKills = M.union (pu <$ M.filter (any ((Virus==) . oshape)) allClears) (pVirusKills pre)
			, pLastVirusKill = max (if clears summary > 0 then pu else -1) (pLastVirusKill pre)
			, pPlacements = HM.insertWith (++) pill [pu] (pPlacements pre)
			-- We may have multiple clears at a given location. But for
			-- simplicity, we'll just count the current pill once. This makes
			-- it easier to do exponential rescaling/discounting later.
			, pClearLocation = M.unionWith (++) ([pu] <$ allClears) (pClearLocation pre)
			, pClearPill = case cr of
				NoClear -> pClearPill pre
				Clear{} -> HM.insertWith (++) (content pill) [pu] (pClearPill pre)
			, pFallTime = (if fallTime > 0 then IM.insert pu fallTime else id) (pFallTime pre)
			}
	where
	allClearsFor = \case
		NoClear -> M.empty
		Clear m dr -> M.unionWith (++) (pure <$> m) $ case dr of
			NoDrop -> M.empty
			Drop _ cr' -> allClearsFor cr'
	fallTimeFor = \case
		-- it's supposed to be the case that m is non-empty, but we'll defend
		-- against bugs by not assuming that
		Clear _ (Drop m cr') -> maximum (0:map fst (M.elems m)) + fallTimeFor cr'
		_ -> 0

data Prediction = Prediction
	{ pVirusKillWeight :: Map Position CFloat
	, pPlacementWeight :: HashMap Pill CFloat
	, pClearLocationWeight :: Map Position CFloat
	, pClearPillWeight :: HashMap PillContent CFloat
	, pOccupied :: HashSet Position
	, pFallWeight :: CUChar
	} deriving (Eq, Ord, Read, Show, Generic, ToJSON, FromJSON)

instance ToEndpoint Prediction where
	toEndpoint = toEndpointRecord
		$   "virus kills" :=: ZeroDefault . pVirusKillWeight
		:&: "pill placements" :=: ZeroDefault . pPlacementWeight
		:&: "clear locations" :=: ZeroDefault . pClearLocationWeight
		:&: "clearing pills" :=: ZeroDefault . pClearPillWeight
		:&: "final occupation" :=: pOccupied
		:&: "fall weight" :=: pFallWeight

deriving via Word8 instance   ToJSON CUChar
deriving via Word8 instance FromJSON CUChar
deriving via Float instance   ToJSON CFloat
deriving via Float instance FromJSON CFloat

prediction :: Preview -> Int -> Prediction
prediction pre = \pu -> Prediction
	{ pVirusKillWeight = discountInt 0.9 pu <$> pVirusKills pre
	, pPlacementWeight = discountList 0.9 pu <$> pPlacements pre
	, pClearLocationWeight = discountList 0.9 pu <$> pClearLocation pre
	, pClearPillWeight = discountList 0.9 pu <$> pClearPill pre
	, pOccupied = occupied
	-- min 255 should never do anything
	, pFallWeight = fromIntegral . min 255 . IM.findWithDefault 0 pu . pFallTime $ pre
	} where
	occupied = ofoldMapWithKey (\pos c -> case c of Empty -> HS.empty; _ -> HS.singleton pos) (pFinalBoard pre)
	discountInt rate pu pu' = if pu > pu' then 0 else rate^(pu'-pu)
	discountList rate pu = min 1 . sum . map (discountInt rate pu)

-- A Haskell version of all the data that goes into the tensor files we save.
data HSTensor = HSTensor
	{ hstBoard :: Board
	, hstPrediction :: Prediction
	, hstScalars :: [Float]
	, hstLookahead :: Lookahead
	, hstPriors :: HashMap Pill Float
	, hstValuation :: Float
	} deriving (Eq, Ord, Read, Show, Generic, ToJSON, FromJSON)

instance ToEndpoint HSTensor where
	toEndpoint = toEndpointRecord
		$   "board" :=: hstBoard
		:&: "prediction" :=: hstPrediction
		:&: "frames" :=: (!!0) . hstScalars
		:&: "original virus count" :=: (!!1) . hstScalars
		:&: "lookahead (left)" :=: OneHotScalar . leftColor . hstLookahead
		:&: "lookahead (right)" :=: OneHotScalar . rightColor . hstLookahead
		:&: "priors" :=: hstPriors
		:&: "valuation" :=: hstValuation

data SaveTensorsSummary = SaveTensorsSummary
	{ stsTensorsSaved :: Integer
	, stsVirusesOriginal :: Int
	, stsVirusesKilled :: Int
	, stsFrames :: Int
	} deriving (Eq, Ord, Read, Show)

-- constructor names are CP + cycle notation
data ColorPermutation = CP | CPBR | CPBY | CPRY | CPBRY | CPBYR
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

class Permutable a where permuteColors :: ColorPermutation -> a -> a

instance Permutable Color where
	permuteColors = \case
		CP -> id
		CPBR  -> \case Blue -> Red   ; Red -> Blue  ; Yellow -> Yellow
		CPBY  -> \case Blue -> Yellow; Red -> Red   ; Yellow -> Blue
		CPRY  -> \case Blue -> Blue  ; Red -> Yellow; Yellow -> Red
		CPBRY -> \case Blue -> Red   ; Red -> Yellow; Yellow -> Blue
		CPBYR -> \case Blue -> Yellow; Red -> Blue  ; Yellow -> Red

instance Permutable Lookahead where
	permuteColors cp lk = Lookahead
		{  leftColor = permuteColors cp ( leftColor lk)
		, rightColor = permuteColors cp (rightColor lk)
		}

instance Permutable PillContent where
	permuteColors cp pc = pc
		{ bottomLeftColor = permuteColors cp (bottomLeftColor pc)
		,      otherColor = permuteColors cp (     otherColor pc)
		}

instance Permutable Cell where
	permuteColors cp = \case
		Empty -> Empty
		Occupied c s -> Occupied (permuteColors cp c) s

instance Permutable Pill where
	permuteColors cp pill = pill { content = permuteColors cp (content pill) }

instance Permutable Board where
	permuteColors cp b = b { cells = V.map (VU.map (permuteColors cp)) (cells b) }

instance (Permutable a, Permutable b) => Permutable (a, b) where
	permuteColors cp (a, b) = (permuteColors cp a, permuteColors cp b)

instance Permutable Prediction where
	permuteColors cp pred = pred
		{ pPlacementWeight = HM.mapKeys (permuteColors cp) (pPlacementWeight pred)
		, pClearPillWeight = HM.mapKeys (permuteColors cp) (pClearPillWeight pred)
		}

-- TODO: could consider varying the cost model, gravity speed, NES vs SNES pill
-- distribution, NES vs SNES (vs other?) pathfinding, and then passing info on
-- which choice was made into the net

type GameDetails = ((Board, Bool, CoarseSpeed), [GameStep])

-- | Arguments: directory to save tensors in; directory to save JSON files in;
-- an index; color permutations to use; and the game record. Tensors will be
-- saved in files named @<i>.nst@, @<i+1>.nst@, etc., up to @<i+di-1>.nst@,
-- with @i@ being the second argument and @di@ being the return value. (@nst@
-- is for [N]urse [S]veta [t]ensor.) JSON will be saved similarly, but with
-- extension @.json@. Typical values for the color permutation list are @[CP]@
-- (i.e. only record the exact game given) or @[minBound..maxBound]@ (i.e.
-- record each game position multiple times, once for each way the colors can
-- be swapped out for each other).
saveTensors :: FilePath -> FilePath -> Integer -> [ColorPermutation] -> GameDetails -> IO SaveTensorsSummary
saveTensors tdir jsdir i0 cps_ (b0, steps) = do
	currentState <- initialState b0
	-- summarize mutates its argument, so make a fresh clone to pass to it
	preview <- initialState b0 >>= \gs -> summarize gs (gsMove <$> steps)
	let valuation = realToFrac (pFinalValuation preview) :: CFloat

	reachable <- mallocArray numPriors
	priors <- mallocArray numPriors
	occupied <- mallocArray cellCount
	virusKills <- mallocArray cellCount
	wishlist <- mallocArray (indexCount @PillContent*cellCount)
	clearLocation <- mallocArray cellCount
	clearPill <- mallocArray (indexCount @PillContent)
	cells <- mallocArray boardSize
	lookahead <- mallocArray lookaheadSize
	scalars <- mallocArray numScalars

	-- TODO: can we remove the rots argument to loop entirely?
	let loop i rots [] = pure (i-i0)
	    loop i rots (gs:gss) = case gsMove gs of
	    	-- subtlety: go calls dmPlay, which records (l, r)
	    	RNG l r -> do
	    		-- TODO: could record a few moves after the kill, too, maybe? but... why?
	    		done <- (pLastVirusKill preview >=) <$> readIORef (pillsUsed currentState)
	    		pu <- readIORef (pillsUsed currentState)
	    		if pu <= pLastVirusKill preview
	    			then go i . HM.fromListWith (++) $ zipWith
	    				(\iRot pc -> (pc, [iRot]))
	    				[0..3]
	    				(iterate (`rotateContent` Clockwise) (PillContent startingOrientation l r))
	    			else pure (i-i0)
	    	Placement bm pill -> do
	    		-- first the bits that are the same for all color permutations
	    		zeroArray cellCount occupied
	    		zeroArray cellCount virusKills
	    		zeroArray cellCount clearLocation
	    		zeroArray numPriors reachable
	    		-- zeroing the priors is probably not needed, since we store a
	    		-- reachable-mask that tells us how to ignore uninitialized
	    		-- values, but let's just do a bit of defensive programming
	    		zeroArray numPriors priors

	    		pred <- prediction preview <$> readIORef (pillsUsed currentState)
	    		hsScalars <- renderScalars scalars currentState
	    		unpermutedBoard <- mfreeze (board currentState)
	    		unpermutedLookbehind <- readIORef (lookbehind currentState)
	    		let scaling = fromIntegral (length rots) / (max 1 (fromIntegral (sum (length <$> rots)) * visitCount (gsRoot gs)))
	    		    hsScaling = recip . max 1 . visitCount $ gsRoot gs

	    		for_ (pOccupied pred) $ \pos -> pokeElemOff occupied (iPos pos) 1
	    		flip  M.traverseWithKey (pVirusKillWeight     pred) $ pokeElemOff virusKills    . iPos
	    		flip  M.traverseWithKey (pClearLocationWeight pred) $ pokeElemOff clearLocation . iPos
	    		flip HM.traverseWithKey (gsChildren gs) $ \move stats -> case move of
	    			RNG{} -> hPutStrLn stderr "WARNING: ignoring RNG move that is a sibling of a Placement move"
	    			Placement bm' pill' -> for_ (HM.findWithDefault [] (content pill') rots) $ \iRot -> do
	    				let j = shiftL iRot logCellCount + iPos (bottomLeftPosition pill')
	    				pokeElemOff reachable j 1
	    				-- TODO: temperature, maybe? (don't forget to fix scaling appropriately)
	    				pokeElemOff priors    j (realToFrac (scaling * visitCount stats))

	    		-- now the bits that change for each permutation
	    		for_ cps $ \(di, cp) -> do
	    			zeroArray (shiftL (indexCount @PillContent) logCellCount) wishlist
	    			zeroArray (indexCount @PillContent) clearPill
	    			zeroArray boardSize cells
	    			zeroArray lookaheadSize lookahead

	    			-- TODO: why is the root's visit count always one bigger than the sum of the children's visit counts?
	    			-- for double pills, we put half the probability mass in each of the two rotations that give the same result
	    			let permutedPriors = HM.fromList [(permuteColors cp pill, hsScaling * visitCount stats) | (Placement _ pill, stats) <- HM.toList (gsChildren gs)]
	    			    permutedBoard = permuteColors cp unpermutedBoard
	    			    permutedLookbehind = permuteColors cp unpermutedLookbehind
	    			    permutedPred = permuteColors cp pred
	    			    hst = HSTensor
	    			    	{ hstBoard = permutedBoard
	    			    	, hstPrediction = permutedPred
	    			    	, hstScalars = hsScalars
	    			    	, hstLookahead = permutedLookbehind
	    			    	, hstPriors = permutedPriors
	    			    	, hstValuation = pFinalValuation preview
	    			    	}

	    			flip HM.traverseWithKey (pPlacementWeight permutedPred) $ \pill -> pokeElemOff wishlist (shiftL (toIndex (content pill)) logCellCount + iPos (bottomLeftPosition pill))
	    			flip HM.traverseWithKey (pClearPillWeight permutedPred) $ pokeElemOff clearPill . toIndex
	    			renderBoard cells permutedBoard
	    			renderLookahead lookahead permutedLookbehind

	    			encodeFile (jsdir </> show (i+di) <.> "json") hst
	    			withCString (tdir </> show (i+di) <.> "nst") $ \path ->
	    				cxx_save_example path reachable priors valuation (pFallWeight permutedPred) occupied virusKills wishlist clearLocation clearPill cells lookahead scalars
	    		go (i+diMax) rots
	    	where go i' rots' = dmPlay currentState (gsMove gs) >> loop i' rots' gss

	di <- loop i0 HM.empty steps

	free reachable
	free priors
	free occupied
	free virusKills
	free wishlist
	free clearLocation
	free clearPill
	free cells
	free lookahead
	free scalars

	pure SaveTensorsSummary
		{ stsTensorsSaved = di
		, stsVirusesOriginal = originalVirusCount currentState
		, stsVirusesKilled = M.size (pVirusKills preview)
		, stsFrames = pTotalFrames preview
		}
	where
	cps = zip [0 :: Integer ..] cps_
	diMax = toInteger (length cps)

-- could imagine a OneHot instance for Position instead of this, but given the
-- dependence on board size it seems weird and maybe even unwise
iPos :: Position -> Int
iPos pos = shiftL (x pos) logBoardHeight + y pos

data LossType = LossPriors | LossValuation | LossFallTime | LossOccupied | LossVirusKills | LossWishlist | LossClearLocation | LossClearPill deriving (Bounded, Enum, Eq, Ord, Read, Show)
instance Universe LossType
instance Finite LossType

describeLossType :: LossType -> String
describeLossType = \case
	LossPriors -> "priors"
	LossValuation -> "outcome"
	LossFallTime -> "fall time"
	LossOccupied -> "final occupation"
	LossVirusKills -> "virus kills"
	LossWishlist -> "future placements"
	LossClearLocation -> "clear locations"
	LossClearPill -> "clearing pill"

withLossScaling :: (LossType -> Float) -> (Ptr CFloat -> IO a) -> IO a
withLossScaling scaling = Foreign.withArray [realToFrac (scaling ty) :: CFloat | ty <- [minBound..]]

netTrain :: Net -> Optimizer -> Batch -> (LossType -> Float) -> IO Float
netTrain net_ optim_ batch_ scaling_ = withUnwrapped (net_, (batch_, optim_)) $ \(net, (batch, optim)) ->
	withLossScaling scaling_ $ \scaling ->
		realToFrac <$> cxx_train_net net optim batch scaling

boardWidth, boardHeight, cellCount, boardSize, lookaheadSize, rotations, numPriors, numScalars, lossTypes :: Int
logBoardWidth, logBoardHeight, logCellCount, logRotations, logNumPriors :: Int
boardWidth = 8; logBoardWidth = 3
boardHeight = 16; logBoardHeight = 4
rotations = 4; logRotations = 2
cellCount = boardWidth*boardHeight; logCellCount = logBoardWidth+logBoardHeight
boardSize = (indexCount @Shape + indexCount @Color)*cellCount
lookaheadSize = 2*indexCount @Color
numPriors = rotations*cellCount; logNumPriors = logRotations+logCellCount
-- TODO: really should add gravity here
numScalars = 6 -- frames, log(frames), sqrt(frames), starting viruses, log(starting viruses), 1/sqrt(starting viruses) (in that order)
lossTypes = 1 + fromEnum (maxBound :: LossType)

mallocZeroArray :: Storable a => Int -> IO (Ptr a)
mallocZeroArray n = mallocArray n >>= zeroArray n

zeroArray :: Storable a => Int -> Ptr a -> IO (Ptr a)
zeroArray n ptr = ptr <$ fillBytes ptr 0 (arraySize ptr n)

plusForeignPtr' :: Storable a => ForeignPtr a -> Int -> ForeignPtr a
plusForeignPtr' ptr n = plusForeignPtr ptr (arraySize ptr n)

plusArray :: Storable a => Ptr a -> Int -> Ptr a
plusArray ptr n = plusPtr ptr (arraySize ptr n)

arraySize :: forall a ptr. Storable a => ptr a -> Int -> Int
arraySize ptr n = n * sizeOf (undefined :: a)

mallocForeignPtrArrays :: Storable a => [Int] -> IO [ForeignPtr a]
mallocForeignPtrArrays lengths = do
	base <- mallocForeignPtrArray (sum lengths)
	pure . init $ scanl plusForeignPtr' base lengths

foreign import ccall "next_sample_net" cxx_next_sample_net :: Bool -> Ptr CStructure -> Ptr CStructure -> IO (Ptr NextNet)
foreign import ccall "&next_discard_net" cxx_next_discard_net :: FinalizerPtr NextNet
foreign import ccall "next_evaluate_net" cxx_next_evaluate_net :: Ptr NextNet -> Ptr CEndpoint -> IO (Ptr CEndpoint)
foreign import ccall "next_introspect_net" cxx_next_introspect_net :: Ptr NextNet -> IO (Ptr CEndpoint)

newtype NextNet = NextNet (ForeignPtr NextNet) deriving newtype CWrapper

gcNextNet :: Ptr NextNet -> IO NextNet
gcNextNet ptr = NextNet <$> newForeignPtr cxx_next_discard_net ptr

nextNetSample' :: Bool -> Structure -> Structure -> IO NextNet
nextNetSample' training i o = do
	c_i <- cStructure i
	c_o <- cStructure o
	withUnwrapped (c_i, c_o) \(ptr_i, ptr_o) -> cxx_next_sample_net training ptr_i ptr_o >>= gcNextNet

nextNetEvaluation' :: NextNet -> Endpoint -> IO Endpoint
nextNetEvaluation' net_ i = do
	c_i <- cEndpoint i
	withUnwrapped (net_, c_i) \(net, ptr_i) ->
		cxx_next_evaluate_net net ptr_i >>= gcEndpoint >>= hsEndpoint

nextNetSample :: Bool -> IO NextNet
nextNetSample training = nextNetSample' training (structure @NextNetInput) (structure @NextNetOutput)

nextNetEvaluation :: NextNet -> Vector NextNetInput -> IO (Vector NextNetOutput)
nextNetEvaluation net is = fromEndpoint <$> nextNetEvaluation' net (toEndpoint is)

nextNetIntrospect :: NextNet -> IO Endpoint
nextNetIntrospect net_ = withUnwrapped net_ (cxx_next_introspect_net >=> gcEndpoint >=> hsEndpoint)

data NextNetInput = NextNetInput
	{ niBoard :: Board
	, niFrames :: Int
	, niOriginalVirusCount :: Int
	} deriving (Eq, Ord, Read, Show)

instance Structured NextNetInput where
	structure = SDictionary $ tail [undefined
		, ("board", structure @Board)
		, ("frames", STensor Positive [])
		-- Positive isn't really right for log(frames), since it can be as low
		-- as -1, but reporting a leaf type for net *inputs* is a bit odd
		-- anyway and the leaf type will be essentially ignored
		, ("log(frames)", STensor Positive [])
		, ("sqrt(frames)", STensor Positive [])
		, ("original virus count", STensor Positive [])
		, ("log(original virus count)", STensor Positive [])
		, ("1/sqrt(original virus count)", STensor Positive [])
		]

instance ToEndpoint NextNetInput where
	toEndpoint = toEndpointRecord
		$   "board" :=: niBoard
		:&: "frames" :=: frames
		:&: "log(frames)" :=: safeLog . frames
		:&: "sqrt(frames)" :=: sqrt . frames
		:&: "original virus count" :=: viruses
		:&: "log(original virus count)" :=: safeLog . viruses
		:&: "1/sqrt(original virus count)" :=: recip . sqrt . viruses
		where
		frames, viruses :: NextNetInput -> CFloat
		frames = fromIntegral . niFrames
		viruses = fromIntegral . niOriginalVirusCount

data NextGroundTruth = NextGroundTruth
	{ gtPriors :: HashMap Pill Float
	, gtLookahead :: Lookahead
	, gtValuation :: Float
	} deriving (Eq, Ord, Read, Show)

instance ToEndpoint NextGroundTruth where
	toEndpoint = toEndpointRecord
		$   "priors" :=: NoDefault . gtPriors
		:&: "valuation" :=: \gt -> NoDefault (HM.singleton (gtLookahead gt) (gtValuation gt))

data NextNetOutput = NextNetOutput
	{ noPriors :: HashMap Pill Float
	, noValuation :: HashMap Lookahead Float
	-- TODO: poke through the stuff in Prediction and see what we should migrate into here
	} deriving (Eq, Ord, Read, Show)

instance Structured NextNetOutput where
	structure = SDictionary $ tail [undefined
		, ("priors", STensor Categorical (indexCounts @Pill))
		, ("valuation", STensor Unit (indexCounts @Lookahead))
		]

instance FromEndpoint NextNetOutput where
	endpointIndex = endpointIndexRecord $ pure NextNetOutput
		<*> field "priors"
		<*> field "valuation"

data NextTrainingExample = NextTrainingExample
	{ teInput :: NextNetInput
	, teTruth :: NextGroundTruth
	} deriving (Eq, Ord, Read, Show)

instance ToEndpoint NextTrainingExample where
	toEndpoint = toEndpointRecord
		$   "input" :=: teInput
		:&: "ground truth" :=: teTruth

nextTrainingExamples :: GameDetails -> IO (Vector NextTrainingExample)
nextTrainingExamples (b0, steps) = do
	currentState <- initialState b0
	-- summarize mutates its argument, so make a fresh clone to pass to it
	-- TODO: either use more of preview or compute less of preview
	preview <- initialState b0 >>= \gs -> summarize gs (gsMove <$> steps)
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
	    		    		, gtValuation = pFinalValuation preview
	    		    		}
	    		    	}
	    		(te, gss) <$ dmPlay currentState (gsMove gs)

	V.unfoldrExactNM numPlacements loop steps
