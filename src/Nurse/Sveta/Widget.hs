{-# Language DataKinds #-} -- not on by default because of how much extra stuff it creates at the type/kind level

module Nurse.Sveta.Widget (
	-- * Raw drawing grid
	DrawingGrid, newDrawingGrid, dgSetRenderer, dgWidget,
	dgSetSize, dgSetWidth, dgSetHeight,
	dgGetSize, dgGetWidth, dgGetHeight,

	-- * Player state
	PlayerStateModel(..),
	psmBoardL, psmLookaheadL, psmOverlayL,
	PlayerStateView, newPlayerStateView, psvWidget,
	psvGet, psvSet,
	psvModifyM, psvModifyM_, psvModify, psvModify_,

	-- * Hyperparameters
	HyperParametersView,
	newHyperParametersView, hpvWidget, hpvSet,

	-- * Train\/test\/validation split
	ValidationSplit,
	newValidationSplit, vsSet, vsGet, vsSample,
	ValidationSplitView,
	newValidationSplitView, vsvWidget, vsvSet,

	-- * Thread management
	ThreadManager,
	newThreadManager, tmWidget, tmStartThread, tmDieThen,
	ThreadView(..), tvNew,
	StatusCheck(..), scIO_,
	Affinity(..),

	-- * General configuration request machinery
	ConfigurationRequestView(..),
	newConfigurationRequestView,
	crvRequest, crvSet, crvUpdateStatus, crvGridAttributes, crvAttach,

	-- * Grids of objects all with the same size
	HomogeneousGridModel(..), HomogeneousGridView,
	newHomogeneousGridView, hgvWidget, hgvSetModel,

	-- * Watching widget size
	SizeAllocationMonitor, newSizeAllocationMonitor, samWidget,
	samOnResize, samDisconnectHandler,

	-- * Noticing when things change
	Stable,
	newStable,
	sPayload,
	sUpdate, sUpdateM, sTryUpdate, sSet, sTrySet,
	sOnSubterm,

	Tracker,
	newTracker, tWhenUpdated, tWhenUpdated_,

	-- * Utilities
	removeAll,
	tshow, tread,
	) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.GI.Base.Attributes
import Data.GI.Base.Signals
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.IORef
import Data.List
import Dr.Mario.Model as DM
import GI.Cairo.Render
import GI.Cairo.Render.Connector
import GI.GLib
import GI.Gtk as G
import Nurse.Sveta.Cairo
import Nurse.Sveta.STM
import Nurse.Sveta.Tomcats (HyperParameters(..))
import Nurse.Sveta.Util
import System.IO
import System.Random.MWC
import Text.Read

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified GHC.OverloadedLabels as Overload

data DrawingGrid = DG
	{ dgCanvas :: DrawingArea
	, dgFrame :: AspectFrame
	, dgSize :: IORef (Double, Double)
	}

-- | Takes a width and height. You get a 'DrawingArea'-alike thing where, when
-- you're drawing, you get to assume that the visible space ranges from 0-width
-- in the X direction (i.e. 0 is the far left of the widget, and width is the
-- far right of the widget -- so rendering a 1x1 thing starting at width-1 will
-- be fully visible and butt up against the edge) and 0-height in the Y
-- direction.
--
-- Uses the math convention for Y coordinates: bigger numbers are higher than
-- smaller numbers.
newDrawingGrid :: MonadIO m => Double -> Double -> m DrawingGrid
newDrawingGrid w h = do
	da <- new DrawingArea []
	af <- new AspectFrame $ tail [undefined
		, #xalign := 0.5
		, #yalign := 0.5
		, #ratio := realToFrac (w / h)
		, #child := da
		, #obeyChild := False
		, #hexpand := True
		, #vexpand := True
		]
	ref <- liftIO $ newIORef (w, h)
	pure (DG da af ref)

-- TODO: would be nice to plug into gi's attribute mechanism instead of having
-- explicit getters and setters like this... especially if this means we could
-- *also* plug into the mechanism for `new`
dgSetSize :: MonadIO m => DrawingGrid -> Double -> Double -> m ()
dgSetSize dg w h = do
	set (dgFrame dg) [#ratio := realToFrac (w / h)]
	liftIO $ writeIORef (dgSize dg) (w, h)
	#queueDraw dg

dgSetWidth :: MonadIO m => DrawingGrid -> Double -> m ()
dgSetWidth dg w = do
	(_, h) <- liftIO $ readIORef (dgSize dg)
	dgSetSize dg w h

dgSetHeight :: MonadIO m => DrawingGrid -> Double -> m ()
dgSetHeight dg h = do
	(w, _) <- liftIO $ readIORef (dgSize dg)
	dgSetSize dg w h

dgGetSize :: MonadIO m => DrawingGrid -> m (Double, Double)
dgGetSize = liftIO . readIORef . dgSize

dgGetWidth :: MonadIO m => DrawingGrid -> m Double
dgGetWidth = fmap fst . dgGetSize

dgGetHeight :: MonadIO m => DrawingGrid -> m Double
dgGetHeight = fmap snd . dgGetSize

dgSetRenderer :: MonadIO m => DrawingGrid -> Render () -> m ()
dgSetRenderer dg draw = drawingAreaSetDrawFunc (dgCanvas dg) . Just $ \_ ctx _ _ -> flip renderWithContext ctx $ do
	wCanvas <- #getWidth (dgCanvas dg)
	hCanvas <- #getHeight (dgCanvas dg)
	(wGrid, hGrid) <- dgGetSize dg
	initMath wCanvas hCanvas wGrid hGrid
	draw

dgWidget :: MonadIO m => DrawingGrid -> m Widget
dgWidget = toWidget . dgFrame

instance (MonadIO m, a ~ m ()) => Overload.IsLabel "queueDraw" (DrawingGrid -> a) where fromLabel = #queueDraw . dgCanvas

data PlayerStateModel = PSM
	{ psmBoard :: Board
	, psmLookahead :: Maybe Lookahead
	, psmOverlay :: [(Pill, Double)] -- ^ the 'Double's are opacities, 1 for fully visible, 0 for fully transparent
	} deriving (Eq, Ord, Read, Show)

psmBoardL :: PlayerStateModel -> (Board, Board -> PlayerStateModel)
psmBoardL psm = (psmBoard psm, \b -> psm { psmBoard = b })

psmLookaheadL :: PlayerStateModel -> (Maybe Lookahead, Maybe Lookahead -> PlayerStateModel)
psmLookaheadL psm = (psmLookahead psm, \l -> psm { psmLookahead = l })

psmOverlayL :: PlayerStateModel -> ([(Pill, Double)], [(Pill, Double)] -> PlayerStateModel)
psmOverlayL psm = (psmOverlay psm, \o -> psm { psmOverlay = o })

psmRender :: PlayerStateModel -> Render ()
psmRender psm = do
	bottleMaybeLookahead (psmBoard psm) (psmLookahead psm)
	for_ (psmOverlay psm) $ \(p, opacity) -> do
		pushGroup
		pill p
		popGroupToSource
		paintWithAlpha opacity

data PlayerStateView = PSV
	{ psvCanvas :: DrawingGrid
	, psvModel :: IORef PlayerStateModel
	}

newPlayerStateView :: MonadIO m => PlayerStateModel -> m PlayerStateView
newPlayerStateView psm = do
	dg <- newDrawingGrid (fromIntegral w) (fromIntegral h)
	ref <- liftIO $ newIORef psm
	dgSetRenderer dg $ liftIO (readIORef ref) >>= psmRender
	psvUpdateHeightRequest dg psm
	pure (PSV dg ref)
	where
	b = psmBoard psm
	(w, h) = bottleSizeRecommendation b

psvUpdateHeightRequest :: MonadIO m => DrawingGrid -> PlayerStateModel -> m ()
psvUpdateHeightRequest dg psm = do
	w <- dgWidget dg
	set w [#heightRequest := fromIntegral (4 * DM.height (psmBoard psm))]

psvWidget :: MonadIO m => PlayerStateView -> m Widget
psvWidget = dgWidget . psvCanvas

psvGet :: MonadIO m => PlayerStateView -> m PlayerStateModel
psvGet = liftIO . readIORef . psvModel

psvSet :: MonadIO m => PlayerStateView -> PlayerStateModel -> m ()
psvSet psv psm = do
	liftIO $ writeIORef (psvModel psv) psm
	psvUpdateHeightRequest (psvCanvas psv) psm
	#queueDraw psv

psvModifyM :: MonadIO m => PlayerStateView -> (PlayerStateModel -> m (PlayerStateModel, a)) -> m a
psvModifyM psv f = do
	psm <- psvGet psv
	(psm', a) <- f psm
	a <$ psvSet psv psm'

psvModifyM_ :: MonadIO m => PlayerStateView -> (PlayerStateModel -> m PlayerStateModel) -> m ()
psvModifyM_ psv f = psvModifyM psv (fmap (flip (,) ()) . f)

psvModify :: MonadIO m => PlayerStateView -> (PlayerStateModel -> (PlayerStateModel, a)) -> m a
psvModify psv f = psvModifyM psv (pure . f)

psvModify_ :: MonadIO m => PlayerStateView -> (PlayerStateModel -> PlayerStateModel) -> m ()
psvModify_ psv f = do
	liftIO $ modifyIORef (psvModel psv) f
	#queueDraw psv

instance (MonadIO m, a ~ m ()) => Overload.IsLabel "queueDraw" (PlayerStateView -> a) where fromLabel = #queueDraw . psvCanvas

data HyperParametersView = HPV
	{ hpvTop :: Grid
	, hpvIterations, hpvMaxLevel :: ConfigurationRequestView Int
	, hpvDiscountRate, hpvC_puct, hpvDirichlet, hpvPriorNoise, hpvMoveNoise
	, hpvRewardVirusClear, hpvRewardOtherClear, hpvRewardWin, hpvRewardLoss
		:: ConfigurationRequestView Float
	}

newHyperParametersView :: HyperParameters -> (HyperParameters -> IO ()) -> IO HyperParametersView
newHyperParametersView hp request = mfix $ \hpv -> do
	grid <- new Grid crvGridAttributes
	cache <- newIORef (hp, hp)

	let newCRV :: _ => _ -> (_ -> a) -> _ -- retain polymorphism
	    newCRV purp field desc check = newConfigurationRequestView (field hp) desc "next time a game starts" purp \req -> do
	    	hpOld <- hpvRequest hpv
	    	let (valid, hpNew) = check hpOld req
	    	valid <$ when valid (request hpNew)
	    newFloatCRV = newCRV InputPurposeNumber
	    newIntCRV = newCRV InputPurposeDigits
	crvIterations   <- newIntCRV   hpIterations       "iterations"                  \hp' n      -> (0 <= n, hp' { hpIterations = n })
	crvMaxLevel     <- newIntCRV   hpMaxLevel         "max level"                   \hp' n      -> (0 <= n && n <= 20, hp' { hpMaxLevel = n })
	crvC_puct       <- newFloatCRV hpC_puct           "c_puct"                      \hp' c_puct -> (True, hp' { hpC_puct = c_puct })
	crvDirichlet    <- newFloatCRV hpDirichlet        "priors noise's uniformity"   \hp' alpha  -> (0 < alpha, hp' { hpDirichlet = alpha })
	crvPriorNoise   <- newFloatCRV hpPriorNoise       "noisiness of priors"         \hp' noise  -> (0 <= noise && noise <= 1, hp' { hpPriorNoise = noise })
	crvMoveNoise    <- newFloatCRV hpMoveNoise        "noisiness of move selection" \hp' noise  -> (0 <= noise && noise <= 1, hp' { hpMoveNoise = noise })
	crvDiscountRate <- newFloatCRV hpDiscountRate     "discount factor (per frame)" \hp' rate   -> (0 <= rate && rate <= 1, hp' { hpDiscountRate = rate })
	crvVirusClear   <- newFloatCRV hpRewardVirusClear "clearing a virus"            \hp' reward -> (True, hp' { hpRewardVirusClear = reward })
	crvOtherClear   <- newFloatCRV hpRewardOtherClear "clearing a non-virus"        \hp' reward -> (True, hp' { hpRewardOtherClear = reward })
	crvWin          <- newFloatCRV hpRewardWin        "winning the game"            \hp' reward -> (True, hp' { hpRewardWin        = reward })
	crvLoss         <- newFloatCRV hpRewardLoss       "losing the game"             \hp' reward -> (True, hp' { hpRewardLoss       = reward })

	#setMarkup (crvDescription crvC_puct) "c<sub>puct</sub>"

	iRef <- newIORef 0
	let i = readIORef iRef >>= \n -> n <$ writeIORef iRef (n+1)
	crvAttach crvMaxLevel grid =<< i
	crvAttach crvIterations grid =<< i
	crvAttach crvC_puct grid =<< i
	crvAttach crvDirichlet grid =<< i
	crvAttach crvPriorNoise grid =<< i
	crvAttach crvMoveNoise grid =<< i
	crvAttach crvDiscountRate grid =<< i
	crvAttach crvVirusClear grid =<< i
	crvAttach crvOtherClear grid =<< i
	crvAttach crvWin grid =<< i
	crvAttach crvLoss grid =<< i

	pure HPV
		{ hpvTop = grid
		, hpvIterations       = crvIterations
		, hpvMaxLevel         = crvMaxLevel
		, hpvC_puct           = crvC_puct
		, hpvDirichlet        = crvDirichlet
		, hpvPriorNoise       = crvPriorNoise
		, hpvMoveNoise        = crvMoveNoise
		, hpvDiscountRate     = crvDiscountRate
		, hpvRewardVirusClear = crvVirusClear
		, hpvRewardOtherClear = crvOtherClear
		, hpvRewardWin        = crvWin
		, hpvRewardLoss       = crvLoss
		}

hpvWidget :: HyperParametersView -> IO Widget
hpvWidget = toWidget . hpvTop

hpvRequest :: HyperParametersView -> IO HyperParameters
hpvRequest hpv = pure HyperParameters
	<*> crvRequest (hpvDiscountRate     hpv)
	<*> crvRequest (hpvC_puct           hpv)
	<*> crvRequest (hpvDirichlet        hpv)
	<*> crvRequest (hpvPriorNoise       hpv)
	<*> crvRequest (hpvMoveNoise        hpv)
	<*> crvRequest (hpvRewardVirusClear hpv)
	<*> crvRequest (hpvRewardOtherClear hpv)
	<*> crvRequest (hpvRewardWin        hpv)
	<*> crvRequest (hpvRewardLoss       hpv)
	<*> crvRequest (hpvIterations       hpv)
	<*> crvRequest (hpvMaxLevel         hpv)

hpvSet :: HyperParametersView -> HyperParameters -> IO ()
hpvSet hpv hpCur = do
	crvSet (hpvIterations       hpv) (hpIterations       hpCur)
	crvSet (hpvMaxLevel         hpv) (hpMaxLevel         hpCur)
	crvSet (hpvDiscountRate     hpv) (hpDiscountRate     hpCur)
	crvSet (hpvC_puct           hpv) (hpC_puct           hpCur)
	crvSet (hpvDirichlet        hpv) (hpDirichlet        hpCur)
	crvSet (hpvPriorNoise       hpv) (hpPriorNoise       hpCur)
	crvSet (hpvMoveNoise        hpv) (hpMoveNoise        hpCur)
	crvSet (hpvRewardVirusClear hpv) (hpRewardVirusClear hpCur)
	crvSet (hpvRewardOtherClear hpv) (hpRewardOtherClear hpCur)
	crvSet (hpvRewardWin        hpv) (hpRewardWin        hpCur)
	crvSet (hpvRewardLoss       hpv) (hpRewardLoss       hpCur)

data ConfigurationRequestView a = CRV
	{ crvDescription :: Label
	, crvEditor :: Entry
	, crvBuffer :: EntryBuffer
	, crvRequestStatus :: Label
	, crvCache :: IORef (a, a)
	, crvPeriod :: T.Text
	}

-- | Arguments:
-- * an initial value
-- * a description of the meaning of the value
-- * a description of when requested values get processed and become current
-- * 'InputPurpose'
-- * a callback that checks a new value for validity; if it's valid (and so
-- returns 'True'), it should record the new value as a request
newConfigurationRequestView ::
	(Eq a, Read a, Show a) =>
	a ->
	T.Text -> T.Text ->
	InputPurpose ->
	(a -> IO Bool) ->
	IO (ConfigurationRequestView a)
newConfigurationRequestView a0 description period purpose callback = do
	cache <- newIORef (a0, a0)
	label <- new Label [#label := description, #halign := AlignStart]
	editor <- new Entry [#inputPurpose := purpose]
	buffer <- G.get editor #buffer
	let text = tshow a0
	set buffer [#text := text]
	status <- new Label [#label := text, #halign := AlignStart]

	let crv = CRV
	    	{ crvDescription = label
	    	, crvEditor = editor
	    	, crvBuffer = buffer
	    	, crvRequestStatus = status
	    	, crvCache = cache
	    	, crvPeriod = " " <> period <> ")"
	    	}

	on editor #activate $ do
		textReq <- G.get buffer #text
		for_ (tread textReq) $ \req -> do
			valid <- callback req
			when valid $ do
				modifyIORef cache $ \(_, cur) -> (req, cur)
				crvUpdateStatus crv

	pure crv

crvRequest :: ConfigurationRequestView a -> IO a
crvRequest crv = fst <$> readIORef (crvCache crv)

crvSet :: (Eq a, Show a) => ConfigurationRequestView a -> a -> IO ()
crvSet crv cur = do
	modifyIORef (crvCache crv) $ \(req, _) -> (req, cur)
	crvUpdateStatus crv

crvUpdateStatus :: (Eq a, Show a) => ConfigurationRequestView a -> IO ()
crvUpdateStatus crv = do
	(req, cur) <- readIORef (crvCache crv)
	set (crvRequestStatus crv) [#label := tshow cur <> if req == cur then mempty else " (will change to " <> tshow req <> crvPeriod crv]

crvGridAttributes :: [AttrOp Grid AttrConstruct]
crvGridAttributes = [#columnSpacing := 7, #rowSpacing := 3]

crvAttach :: ConfigurationRequestView a -> Grid -> Int32 -> IO ()
crvAttach crv grid i = do
	#attach grid (crvDescription   crv) 0 i 1 1
	#attach grid (crvEditor        crv) 1 i 1 1
	#attach grid (crvRequestStatus crv) 2 i 1 1

data ValidationSplit = ValidationSplit
	{ vsSum :: Double
	, vsSplit :: HashMap T.Text Double
	, vsGen :: GenIO
	}

-- | ignores 'vsGen'
instance Eq ValidationSplit where
	vs == vs' = (vsSum vs, vsSplit vs) == (vsSum vs', vsSplit vs')

unsafeNewValidationSplit :: HashMap T.Text Double -> GenIO -> ValidationSplit
unsafeNewValidationSplit split gen = ValidationSplit { vsSum = sum split, vsSplit = split, vsGen = gen }

newValidationSplit :: [(T.Text, Double)] -> IO ValidationSplit
newValidationSplit split
	| any ((<=0) . snd) split = fail "newValidationSplit: initial split must contain only positive numbers"
	| null split = fail "newValidationSplit: initial split must contain at least one category"
	| otherwise = unsafeNewValidationSplit (HM.fromListWith (+) split) <$> createSystemRandom

vsSample :: ValidationSplit -> IO T.Text
vsSample ValidationSplit { vsSum = s, vsSplit = m, vsGen = g } = do
	n <- uniformRM (0, s) g
	go n (HM.toList m)
	where
	go n [] = fail $ "vsSample: somehow a ValidationSplit with no splits got built"
	go _ [(category, _)] = pure category
	go n ((category, weight):cws)
		| n < weight = pure category
		| otherwise = go (n-weight) cws

vsSet :: T.Text -> Double -> ValidationSplit -> ValidationSplit
vsSet category weight vs = case compare weight 0 of
	LT -> vs
	EQ | HM.null split -> vs
	   | otherwise -> unsafeNewValidationSplit split (vsGen vs)
	   where split = HM.delete category (vsSplit vs)
	GT -> unsafeNewValidationSplit (HM.insert category weight (vsSplit vs)) (vsGen vs)

vsGet :: ValidationSplit -> T.Text -> Double
vsGet vs category = HM.findWithDefault 0 category (vsSplit vs)

-- TODO: refactor to use ConfigurationRequestView for DRY
data ValidationSplitView = VSV
	{ vsvTop :: Grid
	, vsvCache :: IORef (ValidationSplit, ValidationSplit)
	, vsvLabels :: HashMap T.Text Label -- TODO: UI for adding and deleting categories
	}

newValidationSplitView :: ValidationSplit -> (ValidationSplit -> IO ()) -> IO ValidationSplitView
newValidationSplitView vs request = do
	grid <- new Grid [#columnSpacing := 7, #rowSpacing := 3]
	cache <- newIORef (vs, vs)
	triples <- flip HM.traverseWithKey (enumerate (vsSplit vs)) $ \category (i, weight) -> do
		title <- mkLabel category
		editor <- new Entry [#inputPurpose := InputPurposeNumber]
		buffer <- G.get editor #buffer
		let text = tshow weight
		set buffer [#text := text]
		display <- mkLabel text

		#attach grid title   0 i 1 1
		#attach grid editor  1 i 1 1
		#attach grid display 2 i 1 1

		pure (editor, buffer, display)

	let vsv = VSV
	    	{ vsvTop = grid
	    	, vsvCache = cache
	    	, vsvLabels = triples <&> \(_, _, display) -> display
	    	}

	flip HM.traverseWithKey triples $ \category (editor, buffer, _) -> on editor #activate $ do
		text <- G.get buffer #text
		for_ (tread text) $ \weight -> do
			(vsReq, vsCur) <- readIORef cache
			let vsReq' = vsSet category weight vsReq
			request vsReq'
			writeIORef cache (vsReq', vsCur)
			vsvDisplay vsv vsReq' vsCur

	pure vsv
	where
	mkLabel t = new Label [#label := t, #halign := AlignStart]

vsvWidget :: ValidationSplitView -> IO Widget
vsvWidget = toWidget . vsvTop

vsvSet :: ValidationSplitView -> ValidationSplit -> IO ()
vsvSet vsv vsCur = do
	(vsReq, _) <- readIORef (vsvCache vsv)
	writeIORef (vsvCache vsv) (vsReq, vsCur)
	vsvDisplay vsv vsReq vsCur

-- | If you see this documentation, please report a bug.
vsvDisplay :: ValidationSplitView -> ValidationSplit -> ValidationSplit -> IO ()
vsvDisplay vsv vsReq vsCur = (()<$) . flip HM.traverseWithKey (vsvLabels vsv) $ \category lbl -> do
	let req = vsGet vsReq category
	    cur = vsGet vsCur category
	set lbl [#label := tshow cur <> if req == cur then mempty else " ( will change to " <> tshow req <> " soon)"]

-- TODO: add a paused state, and start threads out in that state to give people
-- a chance to fiddle with parameters (especially ones that might control
-- whether the program crashes, like how many training examples to use per
-- batch)
data ThreadManager = ThreadManager
	{ tmContainer :: Box
	, tmThreadList :: Box
	, tmAddThread :: Button
	, tmRunningThreads :: IORef [Button]
	, tmDying :: IORef (Maybe (IO ()))
	, tmThreadDescription :: T.Text
	, tmFactory :: IO ThreadView
	, tmAffinity :: Affinity
	}

data ThreadView = ThreadView
	{ tvWidget :: Widget
	, tvRefresh :: IO ()
	, tvCompute :: StatusCheck -> IO ()
	}

-- | Unlike the 'ThreadView' constructor, this is polymorphic over what kind of
-- widget is allowed at the top. It also calls the refreshing action once for
-- you right away.
tvNew :: IsWidget w => w -> IO () -> (StatusCheck -> IO ()) -> IO ThreadView
tvNew top refresh compute = do
	topWidget <- toWidget top
	refresh
	pure ThreadView
		{ tvWidget = topWidget
		, tvRefresh = refresh
		, tvCompute = compute
		}

data StatusCheck = StatusCheck
	{ scIO :: IO () -> IO () -- ^ more efficient, does not block
	, scSTM :: STM () -- ^ less efficient, blocks until you should die
	}

-- | Handy for when you don't need to do any cleanup before shutting down.
scIO_ :: StatusCheck -> IO ()
scIO_ sc = scIO sc (pure ())

data ThreadStatus = Live | Dying | Dead SomeException deriving Show

data DiedSuccessfully = DiedSuccessfully deriving (Eq, Ord, Read, Show, Bounded, Enum)
instance Exception DiedSuccessfully where displayException _ = "died successfully"

data Affinity = Green | OS deriving (Bounded, Enum, Eq, Ord, Read, Show)

fork :: Affinity -> IO () -> IO ThreadId
fork Green = forkIO
fork OS = forkOS

newThreadManager :: T.Text -> Affinity -> IO ThreadView -> IO ThreadManager
newThreadManager nm aff mkView = do
	top <- new Box [#orientation := OrientationVertical]
	lst <- new Box [#orientation := OrientationVertical]
	scr <- new ScrolledWindow [#child := lst, #propagateNaturalHeight := True, #propagateNaturalWidth := True]
	lbl <- new Label [#label := nm <> " threads"]
	add <- new Button $ tail [undefined
		, #iconName := "list-add"
		, #halign := AlignCenter
		]
	run <- newIORef []
	die <- newIORef Nothing
	let tm = ThreadManager top lst add run die nm mkView aff

	#append top lbl
	#append top add
	#append top scr
	on add #clicked (tmStartThread tm)
	pure tm

tmWidget :: ThreadManager -> IO Widget
tmWidget = toWidget . tmContainer

infixr 5 `tmDieThen`
tmDieThen :: ThreadManager -> IO () -> IO ()
tmDieThen tm report = do
	set (tmAddThread tm) [#sensitive := False]
	writeIORef (tmDying tm) (Just report)

	btns <- readIORef (tmRunningThreads tm)
	traverse_ #activate btns
	when (null btns) report

-- ╭╴top╶───────────╮
-- │╭╴sta╶─────────╮│
-- ││╭╴btn╶╮╭╴lbl╶╮││
-- ││╰─────╯╰─────╯││
-- │╰──────────────╯│
-- │╭╴tvw╶─────────╮│
-- │╰──────────────╯│
-- ╰────────────────╯
tmStartThread :: ThreadManager -> IO ()
tmStartThread tm = readIORef (tmDying tm) >>= \case
	Just{} -> T.hPutStrLn stderr $ "WARNING: requested the creation of a new " <> tmThreadDescription tm <> " thread after beginning the shutdown process; ignoring request"
	Nothing -> do
		tv <- tmFactory tm

		top <- new Box [#orientation := OrientationVertical]
		sta <- new Box [#orientation := OrientationHorizontal]
		btn <- new Button []
		lbl <- new Label []

		tsRef <- newTVarIO (newStable Live)
		staTracker <- newTracker
		let updateDisplay ts = do
		    	tWhenUpdated staTracker ts $ \s -> do
		    		set lbl [#label := tshow s]
		    		case s of
		    			Live -> set btn [#iconName := "process-stop", #sensitive := True]
		    			Dying -> set btn [#iconName := "edit-delete", #sensitive := False]
		    			Dead{} -> set btn [#iconName := "edit-delete", #sensitive := True]
		    	tvRefresh tv
		readTVarIO tsRef >>= updateDisplay

		#append sta btn
		#append sta lbl
		#append top sta
		#append top (tvWidget tv)
		#append (tmThreadList tm) top

		on btn #clicked $ do
			set btn [#sensitive := False]
			ts <- readTVarIO tsRef
			case sPayload ts of
				Dead{} -> #remove (tmThreadList tm) top
				_ -> do
					atomically $ modifyTVar tsRef (sSet Dying)
					updateDisplay ts

		timeoutAdd PRIORITY_DEFAULT 30 $ do
			ts <- readTVarIO tsRef
			updateDisplay ts
			case sPayload ts of
				Dead{} -> do
					btns <- delete btn <$> readIORef (tmRunningThreads tm)
					writeIORef (tmRunningThreads tm) btns
					when (null btns) (readIORef (tmDying tm) >>= sequence_)
					pure SOURCE_REMOVE
				_ -> pure SOURCE_CONTINUE

		modifyIORef (tmRunningThreads tm) (btn:)
		() <$ fork (tmAffinity tm) (catch
			(tvCompute tv (tmCheckStatus tsRef))
			(atomically . modifyTVar tsRef . sSet . Dead)
			)

tmCheckStatus :: TVar (Stable ThreadStatus) -> StatusCheck
tmCheckStatus tsRef = StatusCheck
	{ scIO = \cleanup -> do
		ts <- readTVarIO tsRef
		case sPayload ts of
			Dying -> cleanup >> throwIO DiedSuccessfully
			_ -> pure ()
	, scSTM = do
		Stable _ Dying <- readTVar tsRef
		pure ()
	}

data HomogeneousGridModel = HGM
	{ hgmIndividualWidth :: Double
	, hgmIndividualHeight :: Double
	, hgmChildren :: [Widget]
	}

data HomogeneousGridView = HGV
	{ hgvMonitor :: SizeAllocationMonitor
	, hgvContainer :: Box
	, hgvModel :: IORef HomogeneousGridModel
	}

-- | Arguments are the width and height of each child.
newHomogeneousGridView :: MonadIO m => m HomogeneousGridView
newHomogeneousGridView = do
	box <- new Box [#orientation := OrientationVertical, #hexpand := True, #vexpand := True]
	sam <- newSizeAllocationMonitor box
	ref <- liftIO $ newIORef HGM
		{ hgmIndividualWidth = 1
		, hgmIndividualHeight = 1
		, hgmChildren = []
		}

	samOnResize sam (readIORef ref >>= hgvReflow box)
	pure HGV
		{ hgvMonitor = sam
		, hgvContainer = box
		, hgvModel = ref
		}

hgvSetModel :: MonadIO m => HomogeneousGridView -> HomogeneousGridModel -> m ()
hgvSetModel hgv hgm = liftIO do
	writeIORef (hgvModel hgv) hgm
	hgvReflow (hgvContainer hgv) hgm

-- | Internal use only; if you see this in any documentation, please file a bug.
hgvReflow :: MonadIO m => Box -> HomogeneousGridModel -> m ()
hgvReflow box hgm = liftIO do
	w <- #getAllocatedWidth  box
	h <- #getAllocatedHeight box
	let denom = fromIntegral h * hgmIndividualWidth  hgm
	    numer = fromIntegral w * hgmIndividualHeight hgm * fromIntegral (length (hgmChildren hgm))
	    gridWidth = if denom == 0
	    	then 1
	    	else round . sqrt . max 1 $ numer / denom
	removeAll box >>= traverse_ (castTo Box >=> traverse_ removeAll)
	for_ (chunksOf gridWidth (hgmChildren hgm)) \chunk -> do
		hbox <- new Box [#orientation := OrientationHorizontal]
		traverse_ (#append hbox) chunk
		#append box hbox

hgvWidget :: MonadIO m => HomogeneousGridView -> m Widget
hgvWidget = samWidget . hgvMonitor

-- | Split the input into chunks, each no larger than the given maximum size,
-- and where no chunk is more than one element longer than another.
chunksOf :: Int -> [a] -> [[a]]
chunksOf (max 1 -> wMax) as = goBig extras as where
	n = length as
	h = (n + wMax - 1) `quot` wMax
	(wLo, extras) = n `quotRem` max 1 h
	wBig = wLo+1

	goBig 0 xs = goSmall xs
	goBig i xs = b : goBig (i-1) e where (b, e) = splitAt wBig xs
	goSmall [] = []
	goSmall xs = b : goSmall e where (b, e) = splitAt wLo xs

data SizeAllocationMonitor = SAM
	{ samHorizontalArea :: DrawingArea
	, samVerticalArea :: DrawingArea
	, samHorizontalBox :: Box
	, samVerticalBox :: Box
	, samChild :: Widget
	}

newSizeAllocationMonitor :: (IsWidget w, MonadIO m) => w -> m SizeAllocationMonitor
newSizeAllocationMonitor child_ = do
	child <- toWidget child_
	ha <- new DrawingArea [#hexpand := True , #vexpand := False]
	va <- new DrawingArea [#hexpand := False, #vexpand := True ]
	hb <- new Box $ tail [undefined
		, #orientation := OrientationHorizontal
		, #hexpand :=> G.get child #hexpand
		, #widthRequest :=> G.get child #widthRequest
		]
	vb <- new Box $ tail [undefined
		, #orientation := OrientationVertical
		, #vexpand :=> G.get child #vexpand
		, #heightRequest :=> G.get child #heightRequest
		]

	#append vb ha
	#append vb hb
	#append hb va
	#append hb child

	pure (SAM ha va hb vb child)

samOnResize :: MonadIO m => SizeAllocationMonitor -> IO () -> m [SignalHandlerId]
samOnResize sam callback = do
	hid <- on (samHorizontalArea sam) #resize \_ _ -> callback
	vid <- on (samVerticalArea   sam) #resize \_ _ -> callback
	pure [hid, vid]

samDisconnectHandler :: MonadIO m => SizeAllocationMonitor -> [SignalHandlerId] -> m ()
samDisconnectHandler sam [hid, vid] = liftIO do
	disconnectSignalHandler (samHorizontalArea sam) hid
	disconnectSignalHandler (samVerticalArea   sam) vid

samWidget :: MonadIO m => SizeAllocationMonitor -> m Widget
samWidget = toWidget . samVerticalBox

-- | A type for tracking the updates to something that doesn't change very often.
data Stable a = Stable
	-- this generation is in the sense of epoch/number of steps/etc.
	{ generation :: {-# UNPACK #-} !Int
	, sPayload_ :: a
	} deriving (Eq, Ord, Read, Show)

-- This is minBound+1, not minBound, so that Tracker can have a simple state
-- that semantically means it hasn't seen the initial value, while still using
-- (<) for cheap new-ness detection.
newStable :: a -> Stable a
newStable = Stable (minBound+1)

sPayload :: Stable a -> a
sPayload = sPayload_

sUpdate :: (a -> a) -> Stable a -> Stable a
sUpdate f (Stable g p) = Stable (g + 1) (f p)

sUpdateM :: (a -> Maybe a) -> Stable a -> Stable a
sUpdateM f s@(Stable g p) = case f p of
	Nothing -> s
	Just p' -> Stable (g+1) p'

sTryUpdate :: Eq a => (a -> a) -> Stable a -> Stable a
sTryUpdate f s = sTrySet (f (sPayload s)) s

sSet :: a -> Stable a -> Stable a
sSet p s = Stable (generation s + 1) p

sTrySet :: Eq a => a -> Stable a -> Stable a
sTrySet p s = if p == sPayload s then s else Stable (generation s + 1) p

sOnSubterm :: (a -> (b, b -> a)) -> (Stable b -> Stable b) -> Stable a -> Stable a
sOnSubterm lens f sa@(Stable g pa) = if g == g' then sa else Stable g' pa'
	where
	(pb, mk) = lens pa
	Stable g' pb' = f (Stable g pb)
	pa' = mk pb'

-- | Not thread safe.
newtype Tracker = Tracker { generationCacheRef :: IORef Int }

newTracker :: IO Tracker
newTracker = Tracker <$> newIORef minBound

tWhenUpdated :: Tracker -> Stable a -> (a -> IO b) -> IO ()
tWhenUpdated t s f = do
	oldGen <- readIORef (generationCacheRef t)
	when (newGen > oldGen) $ do
		f (sPayload s)
		writeIORef (generationCacheRef t) newGen
	where
	newGen = generation s

tWhenUpdated_ :: Tracker -> Stable a -> IO b -> IO ()
tWhenUpdated_ t s = tWhenUpdated t s . const

removeAll :: MonadIO m => Box -> m [Widget]
removeAll b = widgetGetFirstChild b >>= \case
	Nothing -> pure []
	Just w -> #remove b w >> fmap (w:) (removeAll b)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

tread :: Read a => T.Text -> Maybe a
tread = readMaybe . T.unpack
