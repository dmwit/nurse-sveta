module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State as State
import Data.Aeson
import Data.Char
import Data.Foldable
import Data.IORef
import Data.List
import Data.Ord
import Data.Vector (Vector)
import Dr.Mario.Model
import Nurse.Sveta.Files
import Nurse.Sveta.STM
import Nurse.Sveta.STM.BatchProcessor
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Torch
import Nurse.Sveta.Torch.Semantics
import Nurse.Sveta.Util
import System.Directory
import System.IO
import System.Random.MWC
import System.Random.MWC.Distributions
import System.Random.Stateful (uniformDouble01M)
import Text.Read
import Text.Printf

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Vector as V

main :: IO ()
main = do
	proc <- newProcedure 100
	(net0, _optim) <- netSample
	netRef <- newTVarIO net0
	forkIO $ forever (serviceCalls_ proc . netEvaluation =<< readTVarIO netRef)
	g <- createSystemRandom
	(s, t) <- newRNGTreeFromSeed proc g 0 g
	evalStateT repl ReplState
		{ context = SearchContext
			{ ctxRNG = g
			, ctxState = s
			, ctxEval = proc
			, ctxParams = newHyperParameters
			}
		, tree = Left t
		, net = netRef
		}

data ReplState = ReplState
	{ context :: SearchContext
	, tree :: Either RNGTree MoveTree
	, net :: TVar Net
	}

type Repl = StateT ReplState IO

data ParseResult
	= NoParse
	| SubcommandError String
	| Match (Repl ())

data Command = Command
	{ name :: String
	, help :: String
	, parser :: String -> ParseResult
	}

parseAll :: [Command] -> String -> ([(String, String)], [(String, Repl ())])
parseAll cmds s = mconcat
	[ case parser cmd s of
		NoParse -> ([], [])
		SubcommandError e -> ([(name cmd, e)], [])
		Match act -> ([], [(name cmd, act)])
	| cmd <- cmds
	]

repl :: Repl ()
repl = do
	ms <- liftIO (try @IOException getLine)
	forM_ ms \s -> case parseAll commands s of
		(_, [(_, act)]) -> act
		(_, succs@(_:_)) -> do
			replLn $ "Ambiguous command; matching verbs are " ++ intercalate ", " (map fst succs)
			repl
		([], _) -> do
			replLn $ "No matching verbs. Try help for suggestions."
			repl
		(es, _) -> do
			forM_ es \(nm, e) -> replLn $ "Error while trying to parse a " ++ nm ++ " command: " ++ e
			repl

replOut :: String -> Repl ()
replOut s = liftIO (putStr s >> hFlush stdout)

replLn :: String -> Repl ()
replLn = liftIO . putStrLn

commands :: [Command]
commands = sortOn name [helpC, quitC, boardC, treeC, listC, descendC, mctsC, sampleC, weightsC, rngC]

command_ :: String -> String -> ([String] -> Either String (Repl ())) -> Command
command_ nm h f = Command
	{ name = nm
	, help = h
	, parser = go nm
	} where
	go _ [] = success ""
	go _ (' ':s) = success s
	go [] _ = NoParse
	go (c:cs) (c':cs') = if c == c' then go cs cs' else NoParse
	success s = case f (words s) of
		Left e -> SubcommandError e
		Right act -> Match act

command :: String -> String -> ([String] -> Either String (Repl ())) -> Command
command nm h f = command_ nm h (fmap (>>repl) . f)

commandN_ :: String -> String -> [Int] -> ([String] -> Either String (Repl ())) -> Command
commandN_ nm h lens_ f = command_ nm h \case
	args
		| len `S.member` lens -> f args
		| S.null lens -> Left "this verb can't be made into a command. Complain to the guy who implemented the verb."
		| len < lo -> Left $ "not enough arguments (need " ++ (if S.size lens == 1 then "" else "at least ") ++ show lo ++ ")"
		| len > hi -> Left $ "too many arguments (need " ++ (if S.size lens == 1 then "" else "at most ") ++ show hi ++ ")"
		| otherwise -> case (S.lookupLT len lens, S.lookupGT len lens) of
			(Just lo', Just hi') -> Left $ "wrong argument count (acceptable nearby counts are " ++ show lo' ++ " and " ++ show hi' ++ ")"
		where
		len = length args
		lo = S.findMin lens
		hi = S.findMax lens
	where
	lens = S.fromList lens_

commandN :: String -> String -> [Int] -> ([String] -> Either String (Repl ())) -> Command
commandN nm h lens f = commandN_ nm h lens (fmap (>>repl) . f)

-- R for Right
commandNR_ :: String -> String -> [Int] -> ([String] -> Repl ()) -> Command
commandNR_ nm h lens f = commandN_ nm h lens (Right . f)

commandNR :: String -> String -> [Int] -> ([String] -> Repl ()) -> Command
commandNR nm h lens f = commandN nm h lens (Right . f)

helpC :: Command
helpC = commandNR "help" helpH [0,1] \case
	[] -> replLn $ "Available verbs are " ++ intercalate ", " [name cmd | cmd <- commands, name cmd /= "help"] ++ ".\nUse help <verb> for more info."
	[nm] -> replLn case [(name cmd, help cmd) | cmd <- commands, nm `isPrefixOf` name cmd] of
		[] -> "No matching verbs."
		[(_, h)] -> h
		res -> "Ambiguous help request; matching verbs are " ++ intercalate ", " (map fst res)
	where
	helpH = "help [VERB]\nGives more info about how to use the given VERB, or lists available verbs if no argument is given."

quitC :: Command
quitC = command_ "quit" "Quits. What were you expecting?" \_ -> Right (pure ())

boardC :: Command
boardC = commandNR "board" "Prints the board." [0] \_ ->
	replOut . pp =<< liftIO . mfreeze =<< gets (board . ctxState . context)

treeC :: Command
treeC = commandNR "tree" "Usage: tree [ARG]\nPrints the current MCTS tree. Pass an argument for more verbosity." [0,1] \args -> do
	et <- gets tree
	replLn case (args, et) of
		([ ], Left  t) -> ppRNGTree "" t
		([ ], Right t) -> ppMoveTree "" t
		([_], Left  t) -> ppRNGTreeDebug "" t
		([_], Right t) -> ppMoveTreeDebug "" t

listC :: Command
listC = commandNR "list" "Lists the available moves and the index you can use to select them." [0] \_ -> gets tree >>= \case
	Left t -> replLn $ intercalate ", " [show i ++ ": " ++ ppAeson (fromIndex i :: Lookahead) | i <- [0..8]]
	Right t -> replLn case len of
		0 -> "Sorry brother, game's over"
		_ -> concat (zipWith ppMove [0..] ms)
		where
		ms = moves t
		len = length ms
		ppMove i pill = printf "%2d: %s\t%s"
			i
			(ppPill pill)
			(if i `mod` 4 == 3 && i /= len then "\n" else "" :: String)

descendC :: Command
descendC = commandN "descend" "Usage: descend N\nPlays move N (see also list) and descends into the MCTS tree." [1] \case
	[readMaybe -> Just n] | n >= 0 -> Right do
		rs <- State.get
		t' <- case tree rs of
			Left t  | n < 9 -> Right <$> liftIO (descendRNGTree (context rs) t (fromIndex n))
			Right t | n < length ms -> Left <$> liftIO (descendMoveTree (context rs) t (ms !! n)) where ms = moves t
			other -> other <$ replLn "Move index is out of bounds; try list to see your options"
		put rs { tree = t' }
	[s] -> Left $ show s ++ " does not look like a positive number"

moves :: MoveTree -> [Pill]
moves t = sort (HM.keys (childrenMove t) <> V.toList (fst <$> unexploredMove t))

mctsC :: Command
mctsC = commandN "mcts" "Usage: mcts [N]\nExpand the tree using N (default 1) iterations of MCTS." [0, 1] \case
	[] -> Right go
	[readMaybe -> Just n] | n >= 0 -> Right (replicateM_ n go)
	[s] -> Left $ show s ++ " does not look like a positive number"
	where
	go = do
		rs <- State.get
		t' <- liftIO case tree rs of
			Left t -> Left <$> expandRNGTree (context rs) t
			Right t -> Right <$> expandMoveTree (context rs) t
		put rs { tree = t' }

sampleC :: Command
sampleC = commandN "sample" sampleHelp [0, 1] \case
	[] -> go sampleMove
	['b':_] -> go bestMove
	['s':_] -> go sampleMove
	['u':_] -> go uniformMove
	['w':_] -> go weightedMove
	_ -> Left "METHOD must be one of best, sample, uniform, or weighted"
	where
	go f = Right do
		rs <- State.get
		case tree rs of
			Left _ -> do
				lk <- liftIO (sampleRNG (context rs))
				replLn $ ppAeson lk ++ " (" ++ show (toIndex lk) ++ ")"
			Right t -> liftIO (f (context rs) t) >>= \case
				Nothing -> replLn "Game's already over!"
				Just pill -> replLn (ppPill pill ++ " " ++ ppIndex (findIndex (pill==) (moves t)))
	ppIndex Nothing = "(??)"
	ppIndex (Just n) = "(" ++ show n ++ ")"
	sampleHelp = intercalate "\n" $ tail [undefined
		, "Usage: sample [METHOD]"
		, "Choose a move. Available METHODs are:"
		, "    best        Choose the move that was visited the most times during search."
		, "    weighted    Sample from available moves weighted by visit count."
		, "    uniform     Sample from available moves uniformly at random."
		, "    sample      Choose from among the other methods by flipping a weighted coin."
		]

weightsC :: Command
weightsC = commandNR "weights" weightsHelp [0,1] \case
	[] -> liftIO nsDataDir >>= listAvailable
	[file] -> do
		root <- liftIO nsDataDir
		let fullFile = absFileName root Weights file
		liftIO (doesFileExist fullFile) >>= \case
			True -> do
				netRef <- gets net
				liftIO (netLoadForInference fullFile >>= atomically . writeTVar netRef)
				replLn "✓"
			False -> do
				replLn "That file doesn't exist. Maybe try one of these:"
				listAvailable root
	where
	listAvailable root = liftIO (traverse_ putStrLn . sortOn numberPrefix =<< listDirectory (absDirectoryName root Weights))
	weightsHelp = unlines
		[ "Usage: weights [FILE]"
		, "Use the given neural net weights for evaluation. Paths are relative to Nurse Sveta's"
		, "weights directory. If no file is given, list what's available."
		]

numberPrefix :: String -> (Integer, String)
numberPrefix s = case span isDigit s of
	("", _) -> (-1, s)
	(digits, rest) -> (read digits, rest)

rngC :: Command
rngC = commandN "rng" "Generate a fresh board and reset the search tree. If an argument is given, use it as the max level." [0,1] \case
	[] -> Right (go 20)
	[readMaybe -> Just n] | 0 <= n && n <= 20 -> Right (go n)
	                      | otherwise -> Left "Max level must be in the range 0-20 inclusive."
	[_] -> Left "That argument doesn't look like a number!"
	where
	go n = do
		ctx <- gets context
		(s, t) <- liftIO $ newRNGTreeFromSeed (ctxEval ctx) (ctxRNG ctx) 0 (ctxRNG ctx, n)
		modify \rs -> rs { context = ctx { ctxState = s }, tree = Left t }
