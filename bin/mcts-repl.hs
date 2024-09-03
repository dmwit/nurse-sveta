module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State
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
	(net, _optim) <- netSampleNext
	forkIO $ forever (serviceCalls_ proc (netEvaluationNext net))
	g <- createSystemRandom
	(s, t) <- newRNGTree proc g 0 g
	evalStateT repl ReplState
		{ rng = g
		, game = s
		, tree = Left t
		, net = proc
		}

data ReplState = ReplState
	{ rng :: GenIO
	, game :: GameState
	, tree :: Either RNGTree MoveTree
	, net :: Procedure NetInput NetOutput'
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
replOut = liftIO . putStr

replLn :: String -> Repl ()
replLn = liftIO . putStrLn

commands :: [Command]
commands = sortOn name [helpC, quitC, boardC, treeC]

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

helpC :: Command
helpC = command "help" helpH \case
	[] -> Right . replLn $ "Available verbs are " ++ intercalate ", " [name cmd | cmd <- commands, name cmd /= "help"] ++ ".\nUse help <verb> for more info."
	[nm] -> Right $ replLn case [(name cmd, help cmd) | cmd <- commands, nm `isPrefixOf` name cmd] of
		[] -> "No matching verbs."
		[(_, h)] -> h
		res -> "Ambiguous help request; matching verbs are " ++ intercalate ", " (map fst res)
	_ -> Left $ "You must ask for help one verb at a time because I am lazy."
	where
	helpH = "help [VERB]\nGives more info about how to use the given VERB, or lists available verbs if no argument is given."

quitC :: Command
quitC = command_ "quit" "Quits. What were you expecting?" \_ -> Right (pure ())

boardC :: Command
boardC = command "board" "Prints the board." \case
	[] -> Right $ replOut . pp =<< liftIO . mfreeze =<< gets (board . game)
	_ -> Left "too many arguments"

treeC :: Command
treeC = command "tree" "Usage: tree [ARG]\nPrints the current MCTS tree. Pass an argument for more verbosity." \case
	[] -> Right $ gets tree >>= \case
		Left t -> replLn (ppRNGTree "" t)
		Right t -> replLn (ppMoveTree "" t)
	[_] -> Right $ gets tree >>= \case
		Left t -> replLn (ppRNGTreeDebug "" t)
		Right t -> replLn (ppMoveTreeDebug "" t)
	_ -> Left "too many arguments"
