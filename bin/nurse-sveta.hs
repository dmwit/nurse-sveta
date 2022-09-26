module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List
import Data.Maybe
import Data.Time
import Dr.Mario.Model
import Dr.Mario.Tomcats
import Dr.Mario.Widget
import GI.GLib
import GI.Gtk as G
import System.Environment
import System.IO
import System.Random.MWC

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

main :: IO ()
main = do
	app <- new Application []
	on app #activate $ do
		mainRef <- newIORef (MTRunning [])
		box <- new Box [#orientation := OrientationVertical]
		btn <- new Button $ tail [undefined
			, #iconName := "list-add"
			, #halign := AlignCenter
			, On #clicked (startGenerationThread app mainRef box)
			]
		#append box btn
		replicateM_ 3 (startGenerationThread app mainRef box)
		w <- new Window $ tail [undefined
			, #title := "Nurse Sveta"
			, #application := app
			, #child :=> new ScrolledWindow [#child := box]
			, #defaultWidth := 200
			, #defaultHeight := 1000
			, On #closeRequest $ do
				set btn [#sensitive := False]
				mts <- readIORef mainRef
				case mts of
					MTDying{} -> pure False
					MTRunning btns -> do
						traverse_ #activate btns
						writeIORef mainRef (MTDying (length btns))
						pure . not . null $ btns
			]
		#show w
	args <- getArgs
	() <$ #run app (Just args)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- buttons to activate to stop everything, or else the number of deaths we're waiting for before we quit
data MainThreadStatus = MTRunning [Button] | MTDying Int

data GenerationThreadStatus = GTInitializing | GTComputing | GTDying | GTDead SomeException deriving Show

gtDead :: Exception e => e -> GenerationThreadStatus
gtDead = GTDead . SomeException

-- this generation is in the sense of creation
data GenerationThreadState = GenerationThreadState
	{ status :: Stable GenerationThreadStatus
	, summary :: SearchSummary
	, requestedConfiguration :: SearchConfiguration
	, currentConfiguration :: Stable SearchConfiguration
	} deriving Show

newGenerationThreadState :: SearchConfiguration -> GenerationThreadState
newGenerationThreadState cfg = GenerationThreadState
	{ status = newStable GTInitializing
	, summary = SearchSummary
		{ rootPosition = newStable (PSM (emptyBoard 8 16) Nothing [])
		, speeds = HM.empty
		}
	, requestedConfiguration = cfg
	, currentConfiguration = newStable cfg
	}

onRootPosition :: (Stable PlayerStateModel -> Stable PlayerStateModel) -> GenerationThreadState -> GenerationThreadState
onRootPosition f gts = gts { summary = s { rootPosition = f (rootPosition s) } } where
	s = summary gts

generationThreadView :: Application -> IORef MainThreadStatus -> [Button] -> MVar GenerationThreadState -> IO Widget
generationThreadView app mainRef btns genRef = do
	lbl <- new Label []
	btn <- new Button []
	psv <- psvNew (PSM (emptyBoard 8 16) Nothing [])
	row <- new Box [#orientation := OrientationHorizontal]
	top <- new Box [#orientation := OrientationVertical]

	psvTracker <- newTracker
	staTracker <- newTracker
	cfgTracker <- newTracker

	let displayState gts = do
	    	whenUpdated staTracker (status gts) $ \s -> do
	    		set lbl [#label := tshow s]
	    		case s of
	    			GTInitializing -> set btn [#iconName := "process-stop", #sensitive := False]
	    			GTComputing -> set btn [#iconName := "process-stop", #sensitive := True]
	    			GTDying -> set btn [#iconName := "edit-delete", #sensitive := False]
	    			GTDead _ -> set btn [#iconName := "edit-delete", #sensitive := True]
	    	whenUpdated psvTracker (rootPosition (summary gts)) (psvSet psv)
	readMVar genRef >>= displayState

	writeIORef mainRef (MTRunning (btn:btns))
	#append row btn
	#append row lbl
	#append top row
	psvWidget psv >>= #append top

	on btn #clicked $ do
		set btn [#sensitive := False]
		gts <- takeMVar genRef
		case payload (status gts) of
			GTDead{} -> G.get top #parent >>= traverse (castTo Box) >>= \case
				Just (Just box) -> boxRemove box top
				_ -> fail "the impossible happened: a generation thread's view's parent was not a box"
			_ -> let gts' = gts { status = setPayload GTDying (status gts) } in do
				putMVar genRef gts'
				mts <- readIORef mainRef
				case mts of
					MTDying{} -> pure ()
					MTRunning btns -> writeIORef mainRef (MTRunning (delete btn btns))
				displayState gts'

	timeoutAdd PRIORITY_DEFAULT 30 $ do
		gts <- readMVar genRef
		displayState gts
		case payload (status gts) of
			GTDead{} -> do
				mts <- readIORef mainRef
				SOURCE_REMOVE <$ case mts of
					MTDying n | n <= 1 -> #quit app
					          | otherwise -> writeIORef mainRef (MTDying (n-1))
					MTRunning btns -> writeIORef mainRef (MTRunning (delete btn btns))
			_ -> pure SOURCE_CONTINUE

	toWidget top

startGenerationThread :: Application -> IORef MainThreadStatus -> Box -> IO ()
startGenerationThread app mainRef box = do
	mts <- readIORef mainRef
	case mts of
		MTDying{} -> hPutStrLn stderr "WARNING: requested the creation of a new generation thread after beginning the shutdown process; ignoring request"
		MTRunning btns -> do
			genRef <- newMVar (newGenerationThreadState config)
			generationThreadView app mainRef btns genRef >>= #append box
			() <$ forkIO (catch (go genRef) (reportDeath genRef))
	where
	reportDeath ref e = modifyMVar_ ref (\gts -> pure gts { status = setPayload (GTDead e) (status gts) })
	go ref = createSystemRandom >>= startGame ref
	config = SearchConfiguration { c_puct = 1, iterations = 10000 } -- TODO: be more dynamic
	params = dmParameters config
	startGame ref g = gameLoop where
		gameLoop = initialTree params g >>= uncurry moveLoop
		moveLoop s t = do
			[l, r] <- map toEnum <$> replicateM 2 (uniformR (0, 2) g)
			t' <- unsafeDescend params (RNG l r) s t
			boardSnapshot <- mfreeze (board s)
			modifyMVar_ ref (pure . onRootPosition (setPayload (PSM boardSnapshot (Just (l, r)) [])))
			searchLoop s t' (iterations config)

		searchLoop s t 0 = descend params visitCount s t >>= \case
			Nothing -> gameLoop
			Just (_, t') -> moveLoop s t'

		searchLoop s t n = do
			t' <- mcts params s t
			let move = maximumOn (\_ -> visitCount . statistics) (children t')
			-- if mcts has thrown an error somewhere that matters, force it
			-- before we get into the critical section
			case move of
				Just (Placement _ p, _, _) -> p `seq` pure ()
				_ -> pure ()

			gts <- takeMVar ref
			let status' = flip updateM (status gts) $ \case
			    	GTDying -> Just (gtDead DiedSuccessfully)
			    	GTInitializing -> Just GTComputing
			    	_ -> case move of
			    		Just (Placement _ _, _, _) -> Nothing
			    		Just (rngMove, _, _) -> Just . gtDead . userError $ "weird search loop state: looking at RNG moves like " ++ show rngMove
			    		Nothing -> Nothing
			    gts' = case move of
			    	Just (Placement _ p, _, _) -> onRootPosition (onSubterm psmOverlayL (trySetPayload [(p, 0.3)])) gts
			    	_ -> gts
			putMVar ref gts' { status = status' }

			case (payload status', HM.size (children t')) of
				(GTDead{}, _) -> pure ()
				(_, 0) -> gameLoop
				_ -> searchLoop s t' (n-1)

data SearchSpeed = SearchSpeed
	{ searchStart :: UTCTime
	, searchIterations :: Int
	} deriving (Eq, Ord, Read, Show)

data SearchSummary = SearchSummary
	{ rootPosition :: Stable PlayerStateModel
	, speeds :: HashMap T.Text SearchSpeed
	} deriving (Eq, Ord, Read, Show)

data DiedSuccessfully = DiedSuccessfully deriving (Eq, Ord, Read, Show, Bounded, Enum)
instance Exception DiedSuccessfully where displayException _ = "died successfully"

-- | A type for tracking the updates to something that doesn't change very often.
data Stable a = Stable
	-- this generation is in the sense of epoch/number of steps/etc.
	{ generation :: {-# UNPACK #-} !Int
	, payload :: a
	} deriving (Eq, Ord, Read, Show)

newStable :: a -> Stable a
newStable = Stable (minBound+1)

update :: (a -> a) -> Stable a -> Stable a
update f (Stable g p) = Stable (g + 1) (f p)

updateM :: (a -> Maybe a) -> Stable a -> Stable a
updateM f s@(Stable g p) = case f p of
	Nothing -> s
	Just p' -> Stable (g+1) p'

setPayload :: a -> Stable a -> Stable a
setPayload p s = Stable (generation s + 1) p

trySetPayload :: Eq a => a -> Stable a -> Stable a
trySetPayload p s = if p == payload s then s else Stable (generation s + 1) p

onSubterm :: (a -> (b, b -> a)) -> (Stable b -> Stable b) -> Stable a -> Stable a
onSubterm lens f sa@(Stable g pa) = if g == g' then sa else Stable g' pa'
	where
	(pb, mk) = lens pa
	Stable g' pb' = f (Stable g pb)
	pa' = mk pb'

-- | Not thread safe.
newtype Tracker = Tracker { generationCacheRef :: IORef Int }

newTracker :: IO Tracker
newTracker = Tracker <$> newIORef minBound

whenUpdated :: Tracker -> Stable a -> (a -> IO b) -> IO ()
whenUpdated t s f = do
	oldGen <- readIORef (generationCacheRef t)
	when (newGen > oldGen) $ do
		f (payload s)
		writeIORef (generationCacheRef t) newGen
	where
	newGen = generation s
