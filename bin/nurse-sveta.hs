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
import System.Random.MWC

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

main :: IO ()
main = do
	app <- new Application []
	on app #activate $ do
		box <- new Box [#orientation := OrientationVertical]
		btn <- new Button $ tail [undefined
			, #iconName := "list-add"
			, #halign := AlignCenter
			, On #clicked (startGenerationThread box)
			]
		#append box btn
		replicateM_ 3 (startGenerationThread box)
		w <- new Window $ tail [undefined
			, #title := "Nurse Sveta"
			, #application := app
			, #child :=> new ScrolledWindow [#child := box]
			, #defaultWidth := 200
			, #defaultHeight := 1000
			-- TODO: On #closeRequest (clean up, then exit, or exit immediately if requested a second time)
			]
		#show w
	args <- getArgs
	() <$ #run app (Just args)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

data ThreadStatus = Initializing | Computing | Dying | Dead SomeException deriving Show
data ThreadState = ThreadState
	{ status :: ThreadStatus
	, rootPosition :: Maybe PlayerStateModel
	, generation :: Int -- this generation is in the sense of epoch/number of steps/etc.
	} deriving Show

generationThreadView :: MVar ThreadState -> IO Widget
generationThreadView stateRef = do
	lbl <- new Label []
	btn <- new Button []
	psv <- psvNew (PSM (emptyBoard 8 16) Nothing [])
	row <- new Box [#orientation := OrientationHorizontal]
	top <- new Box [#orientation := OrientationVertical]

	let displayState ts = do
	    	set lbl [#label := tshow (status ts)]
	    	case status ts of
	    		Initializing -> set btn [#iconName := "process-stop", #sensitive := False]
	    		Computing -> set btn [#iconName := "process-stop", #sensitive := True]
	    		Dying -> set btn [#iconName := "edit-delete", #sensitive := False]
	    		Dead _ -> set btn [#iconName := "edit-delete", #sensitive := True]
	    	traverse_ (psvSet psv) (rootPosition ts)
	readMVar stateRef >>= displayState

	#append row btn
	#append row lbl
	#append top row
	psvWidget psv >>= #append top

	on btn #clicked $ do
		set btn [#sensitive := False]
		ts <- takeMVar stateRef
		case status ts of
			Computing -> do
				let ts' = ts { status = Dying }
				putMVar stateRef ts'
				displayState ts'
			Dead{} -> G.get top #parent >>= traverse (castTo Box) >>= \case
				Just (Just box) -> boxRemove box top
				_ -> fail "the impossible happened: a generation thread's view's parent was not a box"

	prevGenRef <- newIORef . generation =<< readMVar stateRef
	timeoutAdd PRIORITY_DEFAULT 30 $ do
		prevGen <- readIORef prevGenRef
		ts <- readMVar stateRef
		unless (prevGen == generation ts) (displayState ts)
		writeIORef prevGenRef (generation ts)
		pure $ case status ts of
			Dead{} -> SOURCE_REMOVE
			_ -> SOURCE_CONTINUE

	toWidget top

-- this generation is in the sense of creation
startGenerationThread :: Box -> IO ()
startGenerationThread box = do
	ref <- newMVar (ThreadState Initializing Nothing minBound)
	generationThreadView ref >>= #append box
	() <$ forkIO (catch (go ref) (reportDeath ref))
	where
	reportDeath ref e = modifyMVar_ ref (\ts -> pure ts { status = Dead e, generation = generation ts + 1 })
	go ref = createSystemRandom >>= startGame ref
	startGame ref g = loop where
		loop = do
			threadDelay 1000000 -- TODO
			ts <- takeMVar ref
			case status ts of
				Dying -> putMVar ref ts { status = Dead (SomeException DiedSuccessfully), generation = generation ts + 1 }
				Initializing -> do
					putMVar ref ts { status = Computing, generation = generation ts + 1 }
					loop
				_ -> do
					putMVar ref ts
					loop

data DiedSuccessfully = DiedSuccessfully deriving (Eq, Ord, Read, Show, Bounded, Enum)
instance Exception DiedSuccessfully where displayException _ = "died successfully"
