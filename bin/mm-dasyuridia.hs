-- debug output first letter guide:
-- (B)oard in machine-readable form
-- (E)rror
-- (I)gnored message passed through from dasyuridia
-- (M)essage acted on and passed through from dasyuridia
-- (P)athfinding arguments or results
-- (R)equest sent to dasyuridia

import Dr.Mario.Pathfinding
import Ms.Mendel

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Nurse.Sveta.Tomcats as Tomcats

main :: IO ()
main = do
	dir <- basedir XdgData
	(_, specs) <- loadPopulationAsSpecs_ dir
	genome <- iFromSpec (V.head specs)
	-- The very first genome evaluation of the program takes a little while.
	-- I'm not 100% sure about why, but at a guess libtorch is a largish
	-- library and it gets loaded lazily. In any case, let's do one straight
	-- away to reduce the likelihood of missing a deadline later.
	evaluate $ iEvaluate genome (emptyBoard 8 16) (V.singleton (emptyBoard 8 16, 0))

	args <- getArgs
	(i, o, e, _p) <- runInteractiveProcess "dasyuridia" args Nothing Nothing
	for_ [i, o, e] \h -> hSetBuffering h LineBuffering
	forkIO . forever $ hGetLine e >>= hPrintf stderr "dasyuridia err: %s\n"
	let possiblyEmit' = possiblyEmit i genome
	fforever state0 \s -> hGetLine o >>= \ln -> case parseEvent ln of
		Just e -> case e of
			EBoard b -> putStrLn ("B " ++ ln) >> possiblyEmit' s { sBoard = Just b }
			ELookahead lk -> putStrLn ("M " ++ show (ELookahead lk)) >> possiblyEmit' s { sLookahead = Just lk }
			ESpeed spd -> putStrLn ("M " ++ show (ESpeed spd)) >> possiblyEmit' s { sSpeed = Just spd }
			ERelax{} -> pure state0
			ENextControl fc -> putStrLn ("M " ++ show (ENextControl fc)) >> pure state0
				{ sControl = Just fc
				, sPills = sPills s
				}
			ELock fc pill -> s <$ printf "I ELock %d %s\n" fc (ppPill pill)
			_ -> s <$ putStrLn ("I " ++ show e)
		Nothing -> s <$ putStrLn ("E " ++ show ln)

data State = State
	{ sBoard :: Maybe Board
	, sLookahead :: Maybe Lookahead
	, sControl :: Maybe FrameCount
	, sSpeed :: Maybe CoarseSpeed
	, sPills :: Int
	} deriving (Eq, Ord, Read, Show)

state0 :: State
state0 = State Nothing Nothing Nothing Nothing 0

possiblyEmit :: Handle -> Individual -> State -> IO State
possiblyEmit h g (State (Just b) (Just lk) (Just fc) (Just spd) pu) = do
	tlt <- iterate (>>= tltVisit g) (tltNew g (b, even fc, spd) lk pu) !! 0
	let bestPath = lrtBestChild (tltChild tlt)
	    request = ppPath fc bestPath
	ppIO b
	printf "P sensitive %s gravity %d\n" (show (even fc)) (gravity spd pu)
	hPutStrLn h request
	printf "R %d: %s\n" (fc + mpPathLength bestPath) request
	pure state0 { sPills = pu + 1 }
possiblyEmit _ _ s = pure s

data PillResultTree = PillResultTree
	{ prtShallowChildren :: [LookaheadResultTree]
	, prtDeepChildren :: [LookaheadResultTree]
	, prtIncomingPill :: Pill
	, prtIncomingPath :: MidPath
	, prtEvaluation :: !Double
	} deriving (Eq, Ord, Read, Show)

data LookaheadResultTree = LookaheadResultTree
	-- TODO: move to NonEmpty to make the invariant known to the compiler?
	{ lrtShallowChildren :: [PillResultTree] -- ^ Invariant: never empty
	, lrtDeepChildren :: [PillResultTree]
	, lrtIncomingLookahead :: Lookahead
	, lrtEvaluation :: !Double
	, lrtBestChild :: MidPath
	} deriving (Eq, Ord, Read, Show)

data TopLevelTree = TopLevelTree
	{ tltChild :: LookaheadResultTree
	, tltGameState :: Tomcats.GameState
	}

tltNew :: Tomcats.GameStateSeed seed => Individual -> seed -> Lookahead -> Int -> IO TopLevelTree
tltNew i seed lk pu = do
	gs <- Tomcats.initialState seed
	writeIORef (Tomcats.pillsUsed gs) pu
	fpBefore <- readIORef (Tomcats.framesPassed gs)
	placements <- HM.toList . (if leftColor lk == rightColor lk then fst else snd) <$> Tomcats.gameStateApproxReachable gs
	pillsAndBoards <- for placements \(mp, path) -> do
		let pill = mpPill mp lk
		-- TODO: there must be some way to share this stuff with ms-mendel
		gs' <- Tomcats.cloneGameState gs
		Tomcats.playMove gs' path pill
		b <- mfreeze (Tomcats.board gs')
		fpAfter <- readIORef (Tomcats.framesPassed gs')
		pure (pill, path, (b, fpAfter-fpBefore))
	pure TopLevelTree
		{ tltChild = lrtNew lk $ zipWith
			(\(pill, path, _bfp) eval -> prtFromEvaluation pill path eval)
			pillsAndBoards
			(V.toList $ iEvaluate i (fst (thrd (head pillsAndBoards))) (V.fromList (thrd <$> pillsAndBoards)))
		, tltGameState = gs
		}

lrtNew :: Lookahead -> [PillResultTree] -> LookaheadResultTree
lrtNew lk prts = LookaheadResultTree
	{ lrtShallowChildren = prts
	, lrtDeepChildren = []
	, lrtIncomingLookahead = lk
	, lrtEvaluation = prtEvaluation best
	, lrtBestChild = prtIncomingPath best
	} where
	best = maximumBy (comparing prtEvaluation) prts

prtFromChildren :: Pill -> MidPath -> [LookaheadResultTree] -> PillResultTree
prtFromChildren pill path lrts = PillResultTree
	{ prtShallowChildren = lrts
	, prtDeepChildren = []
	, prtIncomingPill = pill
	, prtIncomingPath = path
	, prtEvaluation = sum (lrtEvaluation <$> lrts) / fromIntegral (max 1 (length lrts))
	}

prtFromEvaluation :: Pill -> MidPath -> Float -> PillResultTree
prtFromEvaluation pill path eval = PillResultTree
	{ prtShallowChildren = []
	, prtDeepChildren = []
	, prtIncomingPill = pill
	, prtIncomingPath = path
	, prtEvaluation = realToFrac eval
	}

tltVisit :: Individual -> TopLevelTree -> IO TopLevelTree
tltVisit i tlt = do
	gs <- Tomcats.cloneGameState (tltGameState tlt)
	(_, lrt') <- lrtVisit i gs (tltChild tlt)
	pure tlt { tltChild = lrt' }

-- TODO: Right now, evaluations only get updated when we've completed a
-- subtree. We could probably benefit from updating them as we go. If we go
-- that route, we may also want to randomize the order we visit children in.

-- the bool is True if this visit makes the tree have the same depth everywhere
lrtVisit :: Individual -> Tomcats.GameState -> LookaheadResultTree -> IO (Bool, LookaheadResultTree)
lrtVisit i gs lrt = do
	Tomcats.playMove gs (prtIncomingPath prt) (prtIncomingPill prt)
	(expanded, prt') <- prtVisit i gs prt
	pure case (expanded, shallow) of
		(True , []) -> (True , lrtNew lk (prt':deep))
		(True , _ ) -> (False, lrt { lrtShallowChildren = shallow, lrtDeepChildren = prt':deep })
		(False, _ ) -> (False, lrt { lrtShallowChildren = prt':shallow })
	where
	deep = lrtDeepChildren lrt
	prt:shallow = lrtShallowChildren lrt
	lk = lrtIncomingLookahead lrt

-- the bool is True if this visit makes the tree have the same depth everywhere
prtVisit :: Individual -> Tomcats.GameState -> PillResultTree -> IO (Bool, PillResultTree)
prtVisit i gs prt = Tomcats.finished gs >>= \case
	True -> pure (True, prt)
	False -> case prtShallowChildren prt of
		lrt:shallow -> do
			Tomcats.playRNG gs (lrtIncomingLookahead lrt)
			(expanded, lrt') <- lrtVisit i gs lrt
			pure case (expanded, shallow) of
				(True , []) -> (True , prtFromChildren (prtIncomingPill prt) (prtIncomingPath prt) (lrt':prtDeepChildren prt))
				(True , _ ) -> (False, prt { prtShallowChildren = shallow, prtDeepChildren = lrt':prtDeepChildren prt })
				(False, _ ) -> (False, prt { prtShallowChildren = lrt':shallow })
		[] -> do
			(symm_, asymm_) <- Tomcats.gameStateApproxReachable gs
			let symm = HM.toList symm_
			    asymm = HM.toList asymm_
			b <- mfreeze (Tomcats.board gs)
			fpBefore <- readIORef (Tomcats.framesPassed gs)
			-- TODO: A common case is that every pill placement is orientable,
			-- and so pills and their mirrors can share evaluations. Think
			-- about how to take advantage of that.
			pillsAndBoards <- for (liftA2 Lookahead [minBound..maxBound] [minBound..maxBound]) \lk ->
				(,) lk <$> for (if leftColor lk == rightColor lk then symm else asymm) \(mp, path) -> do
					let pill = mpPill mp lk
					gs' <- Tomcats.cloneGameState gs
					Tomcats.playMove gs' path pill
					b' <- mfreeze (Tomcats.board gs')
					fpAfter <- readIORef (Tomcats.framesPassed gs')
					pure (pill, path, (b', fpAfter-fpBefore))
			let evals = V.toList . iEvaluate i b . V.fromList $ [b' | (_, children) <- pillsAndBoards, (_, _, b') <- children]
			    match vs ((lk, pbs):rest) = lrtNew lk (zipWith (\v (pill, path, _) -> prtFromEvaluation pill path v) vs pbs)
			    	: match (drop (length pbs) vs) rest
			    match _ [] = []
			pure . (,) True . prtFromChildren (prtIncomingPill prt) (prtIncomingPath prt) $ match evals pillsAndBoards

tltDepth :: Int -> TopLevelTree -> TopLevelTree
tltDepth n tlt = tlt { tltChild = lrtDepth n (tltChild tlt) }

lrtDepth :: Int -> LookaheadResultTree -> LookaheadResultTree
lrtDepth 0 lrt = lrt { lrtShallowChildren = [], lrtDeepChildren = [] }
lrtDepth n lrt = lrt
	{ lrtShallowChildren = map (prtDepth (n-1)) (lrtShallowChildren lrt)
	, lrtDeepChildren = map (prtDepth (n-1)) (lrtDeepChildren lrt)
	}

prtDepth :: Int -> PillResultTree -> PillResultTree
prtDepth 0 prt = prt { prtShallowChildren = [], prtDeepChildren = [] }
prtDepth n prt = prt
	{ prtShallowChildren = map (lrtDepth (n-1)) (prtShallowChildren prt)
	, prtDeepChildren = map (lrtDepth (n-1)) (prtDeepChildren prt)
	}

ppPath :: FrameCount -> MidPath -> String
ppPath fc = printf "%d %s" fc . concatMap ppStep . mpSteps

ppStep :: MidStep -> String
ppStep = \case
	Blink -> "(<ab)"
	Down -> "v"
	MidStep mdir mrot -> case foldMap ppDirection mdir <> foldMap ppRotation mrot of
		[] -> "e"
		s@[_] -> s
		s -> "(" ++ s ++ ")"

ppDirection :: HDirection -> String
ppDirection = \case L -> "<"; R -> ">"

ppRotation :: Rotation -> String
ppRotation = \case Clockwise -> "a"; Counterclockwise -> "b"

ppTopLevelTree :: TopLevelTree -> String
ppTopLevelTree = ppLookaheadResultTree "" . tltChild

ppLookaheadResultTrees :: String -> [LookaheadResultTree] -> String
ppLookaheadResultTrees indent = unlines . map (ppLookaheadResultTree ('\t':indent))

ppLookaheadResultTree :: String -> LookaheadResultTree -> String
ppLookaheadResultTree indent lrt = printf
	"%s%s => %f (%s)%s"
	indent
	(ppLookahead (lrtIncomingLookahead lrt))
	(lrtEvaluation lrt)
	(drop 2 . ppPath 0 $ lrtBestChild lrt)
	case lrtShallowChildren lrt ++ lrtDeepChildren lrt of
		[] -> "" :: String
		_ -> printf "\n%s%s---\n%s"
			(ppPillResultTrees indent (lrtShallowChildren lrt))
			indent
			(ppPillResultTrees indent (lrtDeepChildren lrt))

ppPillResultTrees :: String -> [PillResultTree] -> String
ppPillResultTrees indent = unlines . map (ppPillResultTree ('\t':indent))

ppPillResultTree :: String -> PillResultTree -> String
ppPillResultTree indent prt = printf
	"%s%s => %f (%s)%s"
	indent
	(ppPill (prtIncomingPill prt))
	(prtEvaluation prt)
	(drop 2 . ppPath 0 $ prtIncomingPath prt)
	case prtShallowChildren prt ++ prtDeepChildren prt of
		[] -> "" :: String
		_ -> printf "\n%s%s---\n%s"
			(ppLookaheadResultTrees indent (prtShallowChildren prt))
			indent
			(ppLookaheadResultTrees indent (prtDeepChildren prt))

type FrameCount = Int
data Event
	= EBoard Board
	| ELookahead Lookahead
	| ESpeed CoarseSpeed
	| ERelax FrameCount
	| ENextControl FrameCount
	| ELock FrameCount Pill
	| EControl FrameCount
	| EAccepted
	| ERejected FrameCount
	| EMalformed
	| EUnprepared
	deriving (Eq, Ord, Read, Show)

parseEvent :: String -> Maybe Event
parseEvent = \case
	c:cs -> M.lookup c tbl >>= ($ cs)
	[] -> Nothing
	where
	-- use Map search for the first character, but linear search for the rest;
	-- the first character almost always uniquely identifies the event anyway
	tbl = M.fromListWith (liftA2 (<|>)) $ tail [ignored
		, "accepted" ~> finished EAccepted
		, "board " ~> \s -> do
			cells <- traverse parseCell (V.fromList s)
			guard (V.length cells == 128)
			pure . EBoard $ unsafeGenerateBoard 8 16 \(Position x y) -> cells V.! (8*y + x)
		, "control " ~> readField EControl
		, "lock " ~> \s -> case words s of
			[frame, orientation, [l, r], '(':x, y] -> do
				pf <- readMaybe frame
				po <- parseOrientation orientation
				pl <- parseColor l
				pr <- parseColor r
				px <- readMaybe =<< dropr ',' x
				py <- readMaybe =<< dropr ')' y
				pure (ELock pf (Pill (PillContent po pl pr) (Position px py)))
			_ -> Nothing
		, "lookahead " ~> \case
			[l, r] -> ELookahead <$> liftA2 Lookahead (parseColor l) (parseColor r)
			_ -> Nothing
		, "malformed" ~> finished EMalformed
		, "next control " ~> readField ENextControl
		, "rejected " ~> readField ERejected
		, "relax " ~> readField ERelax
		, "speed " ~> \case
			"low" -> Just $ ESpeed Low
			"med" -> Just $ ESpeed Med
			"hi" -> Just $ ESpeed Hi
			_ -> Nothing
		, "unprepared" ~> finished EUnprepared
		]

	(c:cs) ~> f = (c, stripPrefix cs >=> f)
	finished v s = v <$ guard (null s)
	readField f = fmap f . readMaybe
	dropr c s = case reverse s of
		c':cs | c == c' -> Just (reverse cs)
		_ -> Nothing

	parseColor = \case
		'b' -> Just Blue
		'r' -> Just Red
		'y' -> Just Yellow
		_ -> Nothing
	
	parseCell 'd' = Just Empty
	parseCell c = Occupied (toEnum color) (toEnum shape)
		<$ guard (color <= colorBound && shape <= shapeBound)
		where
		w = fromEnum c
		color = (w .&. 0b00011) - 1
		shape = (w .&. 0b11100) `shiftR` 2
	colorBound = fromEnum (maxBound :: Color)
	shapeBound = fromEnum (maxBound :: Shape)
	
	parseOrientation = \case
		"horizontal" -> Just Horizontal
		"vertical" -> Just Vertical
		_ -> Nothing
