module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List
import Data.Maybe
import Dr.Mario.Model
import Dr.Mario.Pathfinding
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
					_ -> pure ()
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
	startGame ref g = gameLoop where
		gameLoop = do
			-- TODO: sometimes choose Algorithm H
			seed <- uniformR (2, maxBound) g
			level <- uniformR (0, 20) g
			board <- mrandomBoard seed level
			moveLoop board
		moveLoop board = do
			-- TODO: sometimes choose the NES' pill generation algorithm
			[l, r] <- map toEnum <$> replicateM 2 (uniformR (0, 2) g)
			-- TODO: actually do some AI
			moves <- HM.keys <$> munsafeApproxReachable board (launchPill l r)
			moveIx <- uniformR (0, length moves - 1) g
			mplace board (moves !! moveIx)
			boardSnapshot <- mfreeze board
			gts <- takeMVar ref
			case status gts of
				GTDying -> putMVar ref gts { status = GTDead (SomeException DiedSuccessfully), generation = generation gts + 1 }
				_ -> do
					putMVar ref GenerationThreadState
						{ status = GTComputing
						, rootPosition = Just (PSM boardSnapshot Nothing [])
						, generation = generation gts + 1
						}
					lCell <- munsafeGet board startingBottomLeftPosition
					rCell <- munsafeGet board startingOtherPosition
					if lCell == Empty && rCell == Empty then moveLoop board else gameLoop

data DiedSuccessfully = DiedSuccessfully deriving (Eq, Ord, Read, Show, Bounded, Enum)
instance Exception DiedSuccessfully where displayException _ = "died successfully"
