module Main where

import Control.Exception
import Control.Monad
import Data.Aeson (decodeFileStrict')
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.MultiSet (MultiSet)
import Data.Set (Set)
import Data.Tree
import Dr.Mario.Model
import GHC.IO.Exception
import Nurse.Sveta.Files
import Nurse.Sveta.Tomcats (dmPlay, initialState, GameState(..), Move(..))
import Nurse.Sveta.Torch
import System.Environment
import System.IO
import System.IO.Error

import qualified Data.Set as S
import qualified Data.MultiSet as MS

data Result = Result
	{ lossDistribution :: MultiSet Int
	, clearDistribution :: MultiSet Int
	, noParse :: Set FilePath
	, didNotExist :: Set FilePath
	} deriving (Eq, Ord, Read, Show)

main :: IO ()
main = do
	paths <- getArgs
	resultTree@(Node (_, topResult) _) <- processTop paths
	unless (S.null (noParse topResult)) do
		hPutStrLn stderr "WARNING: some files were ignored because they did not successfully parse as games"
		for_ (noParse topResult) $ \fp ->
			hPutStrLn stderr ("\t" <> fp)
	unless (S.null (didNotExist topResult)) do
		hPutStrLn stderr "WARNING: some files that were expected to exist did not"
		hPutStrLn stderr "\t(possible cause: specified an incorrect path on the command line)"
		hPutStrLn stderr "\t(possible cause: race condition with another process modifying directories while we looked at them)"
		for_ (didNotExist topResult) (hPutStrLn stderr . ("\t"++))
	ppResults 0 resultTree

processTop :: [FilePath] -> IO (Tree (FilePath, Result))
processTop fps = processFiles "" fps <&> \case
	[t] -> forgetDir <$> t
	ts -> Node ("<top>", foldMap result ts) (map (forgetDir <$>) ts)
	where
	result (Node (_, _, r) _) = r
	forgetDir (fp, _, r) = (fp, r)

processFiles :: FilePath -> [FilePath] -> IO (Forest (FilePath, Bool, Result))
processFiles = traverse . processFile

processFile :: FilePath -> FilePath -> IO (Tree (FilePath, Bool, Result))
processFile parent fp = catch (processAsDir parent fp) \case
	(ioeGetErrorType -> InappropriateType) -> catch (processAsFile parent fp) \case
		e | isDoesNotExistError e -> pure $ pure (fp, False, mempty { didNotExist = S.singleton (parent </> fp) })
		  | otherwise -> throwIO e
	e -> throwIO e

processAsDir :: FilePath -> FilePath -> IO (Tree (FilePath, Bool, Result))
processAsDir parent fp = do
	results <- listDirectory fp' >>= processFiles fp'
	pure Node
		{ rootLabel = (fp, True, mconcat [r | Node (_, _, r) _ <- results])
		, subForest = filter (\(Node (_, isDir, _) _) -> isDir) results
		}
	where fp' = parent </> fp

processAsFile :: FilePath -> FilePath -> IO (Tree (FilePath, Bool, Result))
processAsFile parent fp = decodeFileStrict' fp' >>= \case
	Nothing -> pure (produce mempty { noParse = S.singleton fp' })
	Just seed -> produce <$> processGame seed
	where
	fp' = parent </> fp
	produce res = Node (fp, False, res) []

processGame :: GameDetails -> IO Result
processGame (seed, gss0) = do
	state <- initialState seed
	let go vk pu [] = do
	    	vk' <- readIORef (virusesKilled state)
	    	pure $ if vk' == originalVirusCount state
	    		then mempty { clearDistribution = MS.singleton pu }
	    		else mempty { lossDistribution = MS.singleton pu }
	    go vk pu (gs:gss) = do
	    	dmPlay state (gsMove gs)
	    	vk' <- readIORef (virusesKilled state)
	    	case (vk == vk', gsMove gs) of
	    		(True, RNG{}) -> go vk pu gss
	    		(True, Placement{}) -> go vk (pu+1) gss
	    		(False, _) -> (mempty { clearDistribution = MS.singleton pu } <>) <$> go vk' 0 gss
	go 0 0 gss0

ppResults :: Int -> Tree (FilePath, Result) -> IO ()
ppResults n (Node (fp, res) children) = do
	replicateM_ (n-1) (putStr "│ ")
	when (n>0) (putStr "├╴")
	putStrLn $ fp ++ ": clear = " ++ show (MS.toOccurList (clearDistribution res)) ++ "; loss = " ++ show (MS.toOccurList (lossDistribution res))
	traverse_ (ppResults (n+1)) children

instance Semigroup Result where
	Result l c np dne <> Result l' c' np' dne'
		= Result (l <> l') (c <> c') (np <> np') (dne <> dne')
instance Monoid Result where mempty = Result mempty mempty mempty mempty
