module Main where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.IORef
import Dr.Mario.Model
import Nurse.Sveta.STM.BatchProcessor
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Torch
import System.Random.MWC

main :: IO ()
main = do
	proc <- newProcedure 100
	(net, _optim) <- nextNetSample
	forkIO $ forever (serviceCalls_ proc (nextNetEvaluation net))
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
	-- descend
	'd':_ -> descend g cfg params s t >>= \case
		Nothing -> do
			putStrLn "game's already over"
			repl g cfg params s t
		Just (m, t') -> do
			putStrLn $ "making move " ++ show m
			repl g cfg params s t'
	-- help below
	-- mcts
	'm':_ -> repl g cfg params s =<< mcts params s t
	-- quit
	'q':_ -> pure ()
	-- state
	's':_ -> do
		vk <- readIORef (virusesKilled s)
		pu <- readIORef (pillsUsed s)
		fp <- readIORef (framesPassed s)
		lk <- readIORef (lookbehind s)

		putStrLn . filter ('\\'/=) . filter ('"'/=) . show $ encode lk
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
		putStrLn "Options are board, descend, mcts, quit, state, tree, verbose tree"
		repl g cfg params s t

newSearchConfiguration :: SearchConfiguration
newSearchConfiguration = SearchConfiguration
	{ c_puct = 1 -- no idea what A0 did here
	, iterations = 200
	, typicalMoves = 40
	, priorNoise = 1
	, temperature = 0.2
	}
