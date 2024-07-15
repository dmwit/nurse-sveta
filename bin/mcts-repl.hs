module Main where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Foldable
import Data.IORef
import Data.List
import Data.Ord
import Dr.Mario.Model
import Nurse.Sveta.STM.BatchProcessor
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Torch
import Nurse.Sveta.Util
import System.Random.MWC
import System.Random.MWC.Distributions
import Text.Read
import Text.Printf

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

main :: IO ()
main = do
	proc <- newProcedure 100
	(net, _optim) <- netSample
	forkIO $ forever (serviceCalls_ proc (netEvaluation net))
	g <- createSystemRandom
	let cfg = newSearchConfiguration
	    params = dmParameters cfg proc g
	(s, t_) <- initialTree params g
	[l, r] <- map toEnum <$> replicateM 2 (uniformR (0, 2) g)
	t <- unsafeDescend params (RNG l r) s t_
	repl g cfg params s t

repl :: GenIO -> SearchConfiguration -> DMParameters -> GameState -> Tree Statistics Move -> IO ()
repl g cfg params s t = getLine >>= \case
	-- board
	'b':_ -> do
		putStr . pp =<< mfreeze (board s)
		repl g cfg params s t
	-- choose
	'c':_ -> do
		case (temperature cfg, weights) of
			(_, []) -> putStrLn "There are no children or unexplored nodes in the current tree.\n(Perhaps the game is over.)\nNothing"
			(0, _) -> do
				putStrLn "At 0 temperature, so we choose the maximal visit count."
				putStrLn "All moves and visit counts"
				for_ (enumerateMono rawWeights) \(i, (mv, n)) -> printf "\t%2d: %3d %s\n" i (round n :: Int) (ppMove mv)
				printf "Just %s\n" . ppMove . fst $ maximumBy (comparing snd) rawWeights
			_ -> do
				printf "Temperature is %f; exponent is %f.\n" (temperature cfg) tmpExp
				putStrLn "All moves and visit counts:"
				for_ (enumerateMono rawWeights) \(i, (mv, n)) -> printf "\t%2d: %3d %s\n" i (round n :: Int) (ppMove mv)
				putStrLn "All moves and temperature-adjusted weights:"
				for_ (enumerateMono tmpWeights) \(i, (mv, w)) -> printf "\t%2d: %0.3e %s\n" i w (ppMove mv)
				i <- categorical (V.fromList weights) g
				printf "Chose index %d.\n" i
				printf "Just %s\n" . ppMove $ moves !! i
		repl g cfg params s t
		where
		tmpExp = recip (temperature cfg)
		weight stats = realToFrac (1 + visitCount stats) ** tmpExp
		allStats = (statistics <$> children t) `HM.union` unexplored t
		rawWeights = HM.toList (visitCount <$> allStats)
		tmpWeights = HM.toList (weight <$> allStats)
		(moves, weights) = unzip tmpWeights
	-- descend
	'd':ln -> do
		t' <- case readMaybe <$> words ln of
			[_, Just n] -> desc n
			[Just n] -> desc n
			[] -> desc'
			[_] -> desc'
			_ -> t <$ putStrLn "Didn't understand; looks like you passed a move number but it wasn't a number or maybe there was some extraneous stuff after?"
		repl g cfg params s t'
		where
		desc n
			| 0 <= n && n < moveCount t = do
				let m = movesList t !! n
				putStrLn $ "making move " ++ show m
				unsafeDescend params m s t
			| otherwise = t <$ putStrLn ("move number out of bounds (max is " ++ show (moveCount t - 1) ++ ")")
		desc' = descend g cfg params s t >>= \case
			Nothing -> t <$ putStrLn "game's already over"
			Just (m, t') -> t' <$ putStrLn ("making move " ++ show m)
	-- help below
	-- list
	'l':_ -> do
		forZipWithM [0..] (movesList t) \i m -> putStrLn $ show i ++ ": " ++ ppMove m
		repl g cfg params s t
	-- mcts
	'm':ln -> case readMaybe <$> words ln of
		[_, Just n] | n > 0 -> loop n t
		[Just n] | n > 0 -> loop n t
		[] -> loop 1 t
		[_] -> loop 1 t
		_ -> do
			putStrLn "Didn't understand; looks like you passed an iteration count but it wasn't a number or maybe there was some extraneous stuff after?"
			repl g cfg params s t
		where
		loop 0 t = repl g cfg params s t
		loop n t = loop (n-1) =<< mcts params s t
	-- quit
	'q':_ -> pure ()
	-- state
	's':_ -> do
		vk <- readIORef (virusesKilled s)
		pu <- readIORef (pillsUsed s)
		fp <- readIORef (framesPassed s)
		Lookahead l r <- readIORef (lookbehind s)

		putStrLn [ppColor l, ppColor r]
		putStr . pp =<< mfreeze (board s)
		putStrLn
			$  show (originalVirusCount s - vk) ++ "/" ++ show (originalVirusCount s) ++ " viruses; "
			++ show pu ++ " pills; "
			++ show fp ++ " frames; "
			++ show (speed s) ++ " speed"
		putStrLn $ "down would " ++ (if originalSensitive s then "" else "not ") ++ "have worked on frame 0"
		repl g cfg params s t
	-- tree
	't':_ -> ppTreeSparseIO t >> repl g cfg params s t
	-- verbose tree
	'v':_ -> ppTreeIO t >> repl g cfg params s t
	-- help
	_ -> do
		putStrLn "Options are board, choose, descend [move number], list, mcts [iteration count], quit, state, tree, verbose tree"
		repl g cfg params s t

enumerateMono :: [a] -> [(Int, a)]
enumerateMono = zip [0..]

movesList :: Ord move => Tree stats move -> [move]
movesList t = sort (HM.keys (children t) ++ HM.keys (unexplored t))

moveCount :: Tree stats move -> Int
moveCount t = HM.size (children t) + HM.size (unexplored t)

newSearchConfiguration :: SearchConfiguration
newSearchConfiguration = SearchConfiguration
	{ c_puct = 1 -- no idea what A0 did here
	, iterations = 200
	, typicalMoves = 40
	, priorNoise = 1
	, temperature = 0.2
	}
