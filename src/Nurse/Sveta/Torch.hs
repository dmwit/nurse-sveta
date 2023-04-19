{-# Language AllowAmbiguousTypes #-}

module Nurse.Sveta.Torch (
	Net, netSample, netEvaluation, netTrain, netDetailedLoss, netIntrospect,
	netSave, netLoadForInference, netLoadForTraining,
	HSTensor(..), Prediction(..), NetIntrospection(..),
	LossType(..), describeLossType, LossMask, lossMask, fullLossMask,
	Optimizer, newOptimizer,
	Batch, batchLoad,
	GameStep(..), SaveTensorsSummary(..), saveTensors,
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
import Data.Vector (Vector)
import Dr.Mario.Model
import Dr.Mario.Model.Internal
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Util
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- I played with marking these as unsafe. It really hurt performance a lot,
-- plus it made the UI unbearably choppy.
foreign import ccall "sample_net" cxx_sample_net :: Bool -> IO (Ptr Net)
foreign import ccall "&discard_net" cxx_discard_net :: FunPtr (Ptr Net -> IO ())
foreign import ccall "evaluate_net" cxx_evaluate_net :: Ptr Net -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> Ptr CChar -> Ptr CDouble -> IO ()
foreign import ccall "introspect_net" cxx_introspect_net :: Ptr Net -> Ptr Batch -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall "save_example" cxx_save_example :: CString -> Ptr CChar -> Ptr CDouble -> CDouble -> CUChar -> Ptr CChar -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> Ptr CChar -> Ptr CDouble -> IO ()
foreign import ccall "load_batch" cxx_load_batch :: Ptr CString -> CInt -> IO (Ptr Batch)
foreign import ccall "batch_size" cxx_batch_size :: Ptr Batch -> IO CInt
foreign import ccall "&discard_batch" cxx_discard_batch :: FunPtr (Ptr Batch -> IO ())
foreign import ccall "train_net" cxx_train_net :: Ptr Net -> Ptr Optimizer -> Ptr Batch -> CULong -> IO CDouble
foreign import ccall "detailed_loss" cxx_detailed_loss :: Ptr Net -> Ptr CDouble -> Ptr Batch -> IO ()
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

netDetailedLoss :: Net -> Batch -> IO [(LossType, Double)]
netDetailedLoss net_ batch_ = withUnwrapped (net_, batch_) $ \(net, batch) ->
	allocaArray lossTypes $ \out -> do
		cxx_detailed_loss net out batch
		zipWith (\ty w -> (ty, realToFrac w)) [minBound..] <$> peekArray lossTypes out

class OneHot a where
	indexCount :: Int
	toIndex :: a -> Int
	fromIndex :: Int -> a

instance OneHot Color where
	indexCount = 1 + fromEnum (maxBound :: Color)
	toIndex = fromEnum
	fromIndex = toEnum

instance OneHot Shape where
	indexCount = 4
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

instance OneHot Orientation where
	indexCount = 1 + fromEnum (maxBound :: Orientation)
	toIndex = fromEnum
	fromIndex = toEnum

instance OneHot PillContent where
	indexCount = indexCount @Orientation * indexCount @Color * indexCount @Color
	toIndex pc = (toIndex (orientation pc) * indexCount @Color + toIndex (bottomLeftColor pc)) * indexCount @Color + toIndex (otherColor pc)
	fromIndex n = PillContent
		{ orientation = fromIndex o
		, bottomLeftColor = fromIndex l
		, otherColor = fromIndex r
		} where
		(n', r) = n `quotRem` indexCount @Color
		(o , l) = n' `quotRem` indexCount @Color

renderLookahead :: Ptr CChar -> Color -> Color -> IO ()
renderLookahead lookahead l r = do
	pokeElemOff lookahead (toIndex l                    ) 1
	pokeElemOff lookahead (toIndex r + indexCount @Color) 1

renderBoard :: Ptr CChar -> IOBoard -> IO ()
renderBoard board b = do
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

renderScalars :: Ptr CDouble -> GameState -> IO [Double]
renderScalars scalars gs = do
	frames_ <- readIORef (framesPassed gs)
	let [frames, viruses] = fromIntegral <$> [frames_, originalVirusCount gs]
	    safeLog = log . max (exp (-1))
	pokeArray scalars [frames, safeLog frames, sqrt frames, viruses, safeLog viruses, 1/sqrt viruses]
	pure [realToFrac frames, realToFrac viruses]

render :: Traversable t => t (Int, (GameState, Color, Color)) -> IO (Ptr CChar, Ptr CChar, Ptr CDouble)
render itriples = do
	boards <- mallocZeroArray (n * boardSize)
	lookaheads <- mallocZeroArray (n * lookaheadSize)
	scalars <- mallocArray (n * numScalars)
	for_ itriples $ \(i, (gs, l, r)) -> do
		renderBoard     (plusArray boards     (i*    boardSize)) (board gs)
		renderLookahead (plusArray lookaheads (i*lookaheadSize)) l r
		renderScalars   (plusArray scalars    (i*   numScalars)) gs
	pure (boards, lookaheads, scalars)
	where
	n = length itriples

-- It looks like there's a lot of realToFrac calls in this code, but it doesn't
-- matter, because rewrite rules throw them away.
parseForEvaluation :: Int -> (GameState, Color, Color) -> ForeignPtr CDouble -> ForeignPtr CDouble -> IO DetailedEvaluation
parseForEvaluation i (gs, l, r) priors_ valuation_ = withUnwrapped (priors_, valuation_) $ \(priors, valuation) -> do
	v <- peekElemOff valuation i
	p <- forZipWithM [0..rotations-1] (iterate (`rotateContent` Clockwise) (PillContent startingOrientation l r)) $ \numRots pc -> do
		let iNumRots = iPriors + shiftL numRots logCellCount
		v <- V.generateM boardWidth $ \x -> let ix = iNumRots + shiftL x logBoardHeight in
			V.generateM boardHeight $ \y -> let iy = ix + y in
				realToFrac <$> peekElemOff priors iy
		pure (pc, v)

	pure (realToFrac v, HM.fromList p)
	where
	iPriors = shiftL i logNumPriors

-- TODO: I wonder if we could improve throughput by pushing the rendering into the generation thread, like we did with the parsing
netEvaluation :: Traversable t => Net -> t (GameState, Color, Color) -> IO (t DetailedEvaluation)
netEvaluation net_ triples = do
	[priors_, valuation_] <- mallocForeignPtrArrays [shiftL n logNumPriors, n]
	-- TODO: can we avoid a ton of allocation here by pooling allocations of each size -- or even just the largest size, per Net, say?
	(boards, lookaheads, scalars) <- render itriples

	withUnwrapped (net_, (priors_, valuation_)) $ \(net, (priors, valuation)) ->
		cxx_evaluate_net net (fromIntegral n) priors valuation boards lookaheads scalars
	result <- for itriples $ \(i, state) ->
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
			unsafeInterleaveIO (parseForEvaluation i state priors_ valuation_)

	free boards
	free lookaheads
	free scalars

	pure result
	where
	n = length triples
	itriples = enumerate triples

-- TODO: There sure are a lot of really similar types in here, e.g.
-- DetailedEvaluation, Preview, Prediction, HSTensor, not to mention Ptr
-- CDouble -> Ptr CDouble -> Ptr CDouble -> .... It would probably be good to
-- take a step back and think about abstraction boundaries and APIs a bit, then
-- refactor this stuff to be a bit more coherent/consistent.
data NetIntrospection = NetIntrospection
	{ niPriors :: Vector (Vector (Vector Double)) -- ^ clockwise rotations, x, y
	, niValuation :: Double
	, niFallTime :: Double
	, niOccupied :: Vector (Vector Double)
	, niVirusKills :: Vector (Vector Double)
	, niPlacements :: HashMap PillContent (Vector (Vector Double))
	, niClearLocation :: Vector (Vector Double)
	, niClearPill :: HashMap PillContent Double
	} deriving (Eq, Ord, Read, Show)

parseForIntrospection :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO NetIntrospection
parseForIntrospection priors valuation fallTime occupied virusKills wishlist clearLocation clearPill = pure NetIntrospection
	<*> V.generateM rotations (\rot -> boardFullOfDoubles (plusArray priors (shiftL rot logCellCount)))
	<*> peekD valuation
	<*> peekD fallTime
	<*> boardFullOfDoubles occupied
	<*> boardFullOfDoubles virusKills
	<*> for pillContentMap (\i -> boardFullOfDoubles (plusArray wishlist (shiftL i logCellCount)))
	<*> boardFullOfDoubles clearLocation
	<*> for pillContentMap (peekArrayD clearPill)
	where
	peekArrayD :: Ptr CDouble -> Int -> IO Double
	peekArrayD arr = fmap realToFrac . peek . plusArray arr

	peekD :: Ptr CDouble -> IO Double
	peekD = fmap realToFrac . peek

	boardFullOfDoubles :: Ptr CDouble -> IO (Vector (Vector Double))
	boardFullOfDoubles arr =
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
	, pPlacements :: HashMap Pill [Int]
	, pClearLocation :: Map Position [Int]
	, pClearPill :: HashMap PillContent [Int]
	, pFallTime :: IntMap Int
	, pFinalBoard :: Board -- ask the net to predict whether spaces are occupied at game end or not
	, pTotalFrames :: Int
	, pFinalValuation :: Double
	} deriving (Eq, Ord, Read, Show)

summarize :: GameState -> [Move] -> IO Preview
summarize gs [] = do
	b <- mfreeze (board gs)
	fp <- readIORef (framesPassed gs)
	v <- evaluateFinalState gs
	pure Preview
		{ pVirusKills = M.empty
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
	{ pVirusKillWeight :: Map Position CDouble
	, pPlacementWeight :: HashMap Pill CDouble
	, pClearLocationWeight :: Map Position CDouble
	, pClearPillWeight :: HashMap PillContent CDouble
	, pOccupied :: HashSet Position
	, pFallWeight :: CUChar
	} deriving (Eq, Ord, Read, Show, Generic, ToJSON, FromJSON)

deriving via Word8  instance   ToJSON CUChar
deriving via Word8  instance FromJSON CUChar
deriving via Double instance   ToJSON CDouble
deriving via Double instance FromJSON CDouble

prediction :: Preview -> Int -> Prediction
prediction pre = \pu -> Prediction
	{ pVirusKillWeight = discountUnscaled 0.9 pu <$> pVirusKills pre
	, pPlacementWeight = discountList 0.9 pu <$> pPlacements pre
	, pClearLocationWeight = discountList 0.9 pu <$> pClearLocation pre
	, pClearPillWeight = discountList 0.9 pu <$> pClearPill pre
	, pOccupied = occupied
	-- min 255 should never do anything
	, pFallWeight = fromIntegral . min 255 . IM.findWithDefault 0 pu . pFallTime $ pre
	} where
	occupied = ofoldMapWithKey (\pos c -> case c of Empty -> HS.empty; _ -> HS.singleton pos) (pFinalBoard pre)
	discountUnscaled rate pu pu' = if pu > pu' then 0 else rate^(pu'-pu)
	discountInt rate pu pu' = (1-rate) * discountUnscaled rate pu pu'
	discountList rate pu = sum . map (discountInt rate pu)

-- A Haskell version of all the data that goes into the tensor files we save.
data HSTensor = HSTensor
	{ hstBoard :: Board
	, hstPrediction :: Prediction
	, hstScalars :: [Double]
	, hstLookahead :: (Color, Color)
	, hstPriors :: HashMap Pill Double
	, hstValuation :: Double
	} deriving (Eq, Ord, Read, Show, Generic, ToJSON, FromJSON)

data SaveTensorsSummary = SaveTensorsSummary
	{ stsTensorsSaved :: Integer
	, stsVirusesOriginal :: Int
	, stsVirusesKilled :: Int
	, stsFrames :: Int
	} deriving (Eq, Ord, Read, Show)

-- TODO: could consider varying the cost model, gravity speed, NES vs SNES pill
-- distribution, NES vs SNES (vs other?) pathfinding, and then passing info on
-- which choice was made into the net

-- | Arguments: directory to save tensors in; directory to save JSON files in;
-- an index; and the game record. Tensors will be saved in files named
-- @<i>.nst@, @<i+1>.nst@, etc., up to @<i+di-1>.nst@, with @i@ being the
-- second argument and @di@ being the return value. (@nst@ is for [N]urse
-- [S]veta [t]ensor.) JSON will be saved similarly, but with extension @.json@.
saveTensors :: FilePath -> FilePath -> Integer -> ((Board, Bool, CoarseSpeed), [GameStep]) -> IO SaveTensorsSummary
saveTensors tdir jsdir i0 (b0, steps) = do
	currentState <- initialState b0
	-- summarize mutates its argument, so make a fresh clone to pass to it
	preview <- initialState b0 >>= \gs -> summarize gs (gsMove <$> steps)
	hsLookahead <- newIORef (error "Nurse.Sveta.Torch.saveTensors: tried to use lookahead colors before they were initialized")
	let valuation = realToFrac (pFinalValuation preview) :: CDouble

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

	let loop i rots [] = pure (i-i0)
	    loop i rots (gs:gss) = case gsMove gs of
	    	RNG l r -> do
	    		zeroArray lookaheadSize lookahead
	    		renderLookahead lookahead l r
	    		writeIORef hsLookahead (l, r)
	    		-- dmPlay doesn't do anything, but we call it anyway to defend
	    		-- against that changing in the future
	    		go i . HM.fromListWith (++) $ zipWith
	    			(\iRot pc -> (pc, [iRot]))
	    			[0..3]
	    			(iterate (`rotateContent` Clockwise) (PillContent startingOrientation l r))
	    	Placement bm pill -> do
	    		pred <- prediction preview <$> readIORef (pillsUsed currentState)

	    		zeroArray numPriors reachable
	    		zeroArray numPriors priors -- probably not needed, but defensive programming
	    		hsScalars <- renderScalars scalars currentState
	    		-- TODO: why is the root's visit count always one bigger than the sum of the children's visit counts?
	    		-- for double pills, we put half the probability mass in each of the two rotations that give the same result
	    		let scaling = fromIntegral (length rots) / (max 1 (fromIntegral (sum (length <$> rots)) * visitCount (gsRoot gs)))
	    		    hsScaling = recip . max 1 . visitCount $ gsRoot gs
	    		    hsPriors = HM.fromList [(pill, hsScaling * visitCount stats) | (Placement _ pill, stats) <- HM.toList (gsChildren gs)]
	    		flip HM.traverseWithKey (gsChildren gs) $ \move stats -> case move of
	    			RNG{} -> hPutStrLn stderr "WARNING: ignoring RNG move that is a sibling of a Placement move"
	    			Placement bm' pill' -> for_ (HM.findWithDefault [] (content pill') rots) $ \iRot -> do
	    				let j = shiftL iRot logCellCount + iPos (bottomLeftPosition pill')
	    				pokeElemOff reachable j 1
	    				-- TODO: temperature, maybe? (don't forget to fix scaling appropriately)
	    				pokeElemOff priors    j (realToFrac (scaling * visitCount stats))

	    		zeroArray cellCount occupied
	    		zeroArray cellCount virusKills
	    		zeroArray (shiftL (indexCount @PillContent) logCellCount) wishlist
	    		zeroArray cellCount clearLocation
	    		zeroArray (indexCount @PillContent) clearPill
	    		for_ (pOccupied pred) $ \pos -> pokeElemOff occupied (iPos pos) 1
	    		flip  M.traverseWithKey (pVirusKillWeight     pred) $ pokeElemOff virusKills . iPos
	    		flip HM.traverseWithKey (pPlacementWeight     pred) $ \pill -> pokeElemOff wishlist (shiftL (toIndex (content pill)) logCellCount + iPos (bottomLeftPosition pill))
	    		flip  M.traverseWithKey (pClearLocationWeight pred) $ pokeElemOff clearLocation . iPos
	    		flip HM.traverseWithKey (pClearPillWeight     pred) $ pokeElemOff clearPill . toIndex

	    		zeroArray boardSize cells
	    		renderBoard cells (board currentState)

	    		hst <- pure HSTensor
	    			<*> mfreeze (board currentState)
	    			<*> pure pred
	    			<*> pure hsScalars
	    			<*> readIORef hsLookahead
	    			<*> pure hsPriors
	    			<*> pure (pFinalValuation preview)

	    		encodeFile (jsdir </> show i <.> "json") hst
	    		withCString (tdir </> show i <.> "nst") $ \path ->
	    			cxx_save_example path reachable priors valuation (pFallWeight pred) occupied virusKills wishlist clearLocation clearPill cells lookahead scalars
	    		go (i+1) rots
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

-- could imagine a OneHot instance for Position instead of this, but given the
-- dependence on board size it seems weird and maybe even unwise
iPos :: Position -> Int
iPos pos = shiftL (x pos) logBoardHeight + y pos

data LossType = LossPriors | LossValuation | LossFallTime | LossOccupied | LossVirusKills | LossWishlist | LossClearLocation | LossClearPill deriving (Bounded, Enum, Eq, Ord, Read, Show)

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

newtype LossMask = LossMask CULong deriving (Eq, Ord, Read, Show)
instance Semigroup LossMask where LossMask m <> LossMask m' = LossMask (m .|. m')
instance Monoid LossMask where mempty = LossMask 0
instance CWrapper LossMask where
	type Unwrapped LossMask = CULong
	withUnwrapped (LossMask m) f = f m

lossMask :: LossType -> LossMask
lossMask = LossMask . bit . fromEnum

fullLossMask :: LossMask
fullLossMask = foldMap' lossMask [minBound..]

netTrain :: Net -> Optimizer -> Batch -> LossMask -> IO Double
netTrain net_ optim_ batch_ mask_ = withUnwrapped (net_, (batch_, (optim_, mask_))) $ \(net, (batch, (optim, mask))) ->
	realToFrac <$> cxx_train_net net optim batch mask

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

class CWrapper a where
	type Unwrapped a
	withUnwrapped :: a -> (Unwrapped a -> IO b) -> IO b

instance CWrapper (ForeignPtr a) where
	type Unwrapped (ForeignPtr a) = Ptr a
	withUnwrapped = withForeignPtr

instance CWrapper () where
	type Unwrapped () = ()
	withUnwrapped _ f = f ()

instance (CWrapper a, CWrapper b) => CWrapper (a, b) where
	type Unwrapped (a, b) = (Unwrapped a, Unwrapped b)
	withUnwrapped (wa, wb) f = withUnwrapped wa $ \a -> withUnwrapped wb $ \b -> f (a, b)

instance CWrapper a => CWrapper [a] where
	type Unwrapped [a] = [Unwrapped a]
	withUnwrapped = withMany withUnwrapped

newtype WrappedCString = WCS String
instance CWrapper WrappedCString where
	type Unwrapped WrappedCString = CString
	withUnwrapped (WCS s) = withCString s

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
