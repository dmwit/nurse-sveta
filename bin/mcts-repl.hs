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
import System.Random.Stateful (uniformDouble01M)
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
		v <- uniformDouble01M g
		printf "Sampled %f uniformly from [0,1]. Current noise probability is %f.\n" v (moveNoise cfg)
		mmove <- case (moves, v < moveNoise cfg^2, v < moveNoise cfg) of
			([], _    , _    ) -> Nothing <$ putStrLn "There are no children or unexplored nodes in the current tree.\n(Perhaps the game is over.)"
			(_ , True , _    ) -> do
				putStrLn "Choosing a move uniformly at random."
				n <- uniformR (0, length moves - 1) g
				printf "Chose move %d (out of %d).\n" n (length moves)
				pure . Just $ moves !! n
			(_ , _    , True ) -> do
				putStrLn "Choosing a move using the visit counts as weights."
				putStrLn "All moves and weights:"
				for_ (enumerateMono (zip moves weights)) \(i, (mv, w)) -> printf "\t%2d: %0.3e %s\n" i w (ppMove mv)
				n <- categorical (V.fromList weights) g
				printf "Chose move %d.\n" n
				pure . Just $ moves !! n
			_ -> do
				putStrLn "Choosing the best move."
				case bestMoves t of
					BestMoves c ms
						| V.length ms == 0 -> do
							putStrLn "There are no best moves?? This seems like it's probably a bug. Falling back to choosing uniformly at random."
							n <- uniformR (0, length moves - 1) g
							printf "Chose move %d (out of %d).\n" n (length moves)
							pure . Just $ moves !! n
						| V.length ms == 1 -> pure . Just $ ms V.! 0
						| otherwise -> do
							printf "The following moves all tied for best, with visit count %f:\n" c
							V.iforM_ ms \i move -> printf "%2d: %s\n" i (ppMove move)
							n <- uniformR (0, V.length ms - 1) g
							printf "Chose move %d.\n" n
							pure . Just $ ms V.! n
		case mmove of
			Nothing -> putStrLn "Nothing"
			Just move -> putStr "Just " >> putStrLn (ppMove move)
		repl g cfg params s t
		where
		weight = realToFrac . (1+) . visitCount
		(moves, weights) = unzip . HM.toList $
			(weight . statistics <$> children t) `HM.union`
			(weight <$> unexplored t)
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
	, typicalMoves = 25
	, priorNoise = 0.1
	, moveNoise = 0.2
	}
