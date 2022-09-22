module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List
import Data.Maybe
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

data GenerationThreadState = GenerationThreadState
	{ status :: GenerationThreadStatus
	, rootPosition :: Maybe PlayerStateModel
	, generation :: Int -- this generation is in the sense of epoch/number of steps/etc.
	} deriving Show

generationThreadView :: Application -> IORef MainThreadStatus -> [Button] -> MVar GenerationThreadState -> IO Widget
generationThreadView app mainRef btns genRef = do
	lbl <- new Label []
	btn <- new Button []
	psv <- psvNew (PSM (emptyBoard 8 16) Nothing [])
	row <- new Box [#orientation := OrientationHorizontal]
	top <- new Box [#orientation := OrientationVertical]

	let displayState gts = do
	    	set lbl [#label := tshow (status gts)]
	    	case status gts of
	    		GTInitializing -> set btn [#iconName := "process-stop", #sensitive := False]
	    		GTComputing -> set btn [#iconName := "process-stop", #sensitive := True]
	    		GTDying -> set btn [#iconName := "edit-delete", #sensitive := False]
	    		GTDead _ -> set btn [#iconName := "edit-delete", #sensitive := True]
	    	traverse_ (psvSet psv) (rootPosition gts)
	readMVar genRef >>= displayState

	writeIORef mainRef (MTRunning (btn:btns))
	#append row btn
	#append row lbl
	#append top row
	psvWidget psv >>= #append top

	on btn #clicked $ do
		set btn [#sensitive := False]
		gts <- takeMVar genRef
		case status gts of
			GTDead{} -> G.get top #parent >>= traverse (castTo Box) >>= \case
				Just (Just box) -> boxRemove box top
				_ -> fail "the impossible happened: a generation thread's view's parent was not a box"
			_ -> let gts' = gts { status = GTDying } in do
				putMVar genRef gts'
				mts <- readIORef mainRef
				case mts of
					MTDying{} -> pure ()
					MTRunning btns -> writeIORef mainRef (MTRunning (delete btn btns))
				displayState gts'

	prevGenRef <- newIORef . generation =<< readMVar genRef
	timeoutAdd PRIORITY_DEFAULT 30 $ do
		prevGen <- readIORef prevGenRef
		gts <- readMVar genRef
		unless (prevGen == generation gts) (displayState gts)
		writeIORef prevGenRef (generation gts)
		case status gts of
			GTDead{} -> do
				mts <- readIORef mainRef
				SOURCE_REMOVE <$ case mts of
					MTDying n | n <= 1 -> #quit app
					          | otherwise -> writeIORef mainRef (MTDying (n-1))
					MTRunning btns -> writeIORef mainRef (MTRunning (delete btn btns))
			_ -> pure SOURCE_CONTINUE

	toWidget top

-- this generation is in the sense of creation
startGenerationThread :: Application -> IORef MainThreadStatus -> Box -> IO ()
startGenerationThread app mainRef box = do
	mts <- readIORef mainRef
	case mts of
		MTDying{} -> hPutStrLn stderr "WARNING: requested the creation of a new generation thread after beginning the shutdown process; ignoring request"
		MTRunning btns -> do
			genRef <- newMVar (GenerationThreadState GTInitializing Nothing minBound)
			generationThreadView app mainRef btns genRef >>= #append box
			() <$ forkIO (catch (go genRef) (reportDeath genRef))
	where
	reportDeath ref e = modifyMVar_ ref (\gts -> pure gts { status = GTDead e, generation = generation gts + 1 })
	go ref = createSystemRandom >>= startGame ref
	config = SearchConfiguration { c_puct = 1, iterations = 10000 } -- TODO: be more dynamic
	params = dmParameters config
	startGame ref g = gameLoop where
		gameLoop = initialTree params g >>= uncurry moveLoop
		moveLoop s t = do
			[l, r] <- map toEnum <$> replicateM 2 (uniformR (0, 2) g)
			t' <- unsafeDescend params (RNG l r) s t
			boardSnapshot <- mfreeze (board s)
			modifyMVar_ ref (\gts -> pure gts { rootPosition = Just (PSM boardSnapshot (Just (l, r)) []) })
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
			let gts' = gts { generation = generation gts + 1 }
			    gts'' = case gts of
			    	GenerationThreadState { status = GTDying } -> gts' { status = gtDead DiedSuccessfully }
			    	GenerationThreadState { status = GTInitializing } -> gts' { status = GTComputing }
			    	GenerationThreadState { rootPosition = Just (PSM b l ps) } -> case move of
			    		Just (Placement _ p, _, _) -> case ps of
			    			[(p', _)] | p == p' -> gts
			    			_ -> gts' { rootPosition = Just (PSM b l [(p, 0.3)] ) }
			    		Just (rngMove, _, _) -> gts' { status = gtDead . userError $ "weird search loop state: looking at RNG moves like " ++ show rngMove }
			    		Nothing -> gts
			    	_ -> gts' { status = gtDead . userError $ "weird search loop state: no root position" }
			putMVar ref gts''

			case (status gts'', HM.size (children t')) of
				(GTDead{}, _) -> pure ()
				(_, 0) -> gameLoop
				_ -> searchLoop s t' (n-1)

data DiedSuccessfully = DiedSuccessfully deriving (Eq, Ord, Read, Show, Bounded, Enum)
instance Exception DiedSuccessfully where displayException _ = "died successfully"
