module Main where

import Data.MultiSet (MultiSet)
import Data.Tree
import GHC.IO.Exception
import Nurse.Sveta.Files
import Nurse.Sveta.Tomcats hiding (Tree)
import Nurse.Sveta.Torch
import Nurse.Sveta.Util

import qualified Data.Set as S
import qualified Data.MultiSet as MS

data Results = Results
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

processTop :: [FilePath] -> IO (Tree (FilePath, Results))
processTop fps = processFiles "" fps <&> \case
	[t] -> forgetDir <$> t
	ts -> Node ("<top>", foldMap result ts) (map (forgetDir <$>) ts)
	where
	result (Node (_, _, r) _) = r
	forgetDir (fp, _, r) = (fp, r)

processFiles :: FilePath -> [FilePath] -> IO (Forest (FilePath, Bool, Results))
processFiles = traverse . processFile

processFile :: FilePath -> FilePath -> IO (Tree (FilePath, Bool, Results))
processFile parent fp = catch (processAsDir parent fp) \case
	(ioeGetErrorType -> InappropriateType) -> catch (processAsFile parent fp) \case
		e | isDoesNotExistError e -> pure $ pure (fp, False, mempty { didNotExist = S.singleton (parent </> fp) })
		  | otherwise -> throwIO e
	e -> throwIO e

processAsDir :: FilePath -> FilePath -> IO (Tree (FilePath, Bool, Results))
processAsDir parent fp = do
	results <- listDirectory fp' >>= processFiles fp'
	pure Node
		{ rootLabel = (fp, True, mconcat [r | Node (_, _, r) _ <- results])
		, subForest = filter (\(Node (_, isDir, _) _) -> isDir) results
		}
	where fp' = parent </> fp

processAsFile :: FilePath -> FilePath -> IO (Tree (FilePath, Bool, Results))
processAsFile parent fp = decodeFileStrict' fp' >>= \case
	Nothing -> pure (produce mempty { noParse = S.singleton fp' })
	Just seed -> produce <$> processGame seed
	where
	fp' = parent </> fp
	produce res = Node (fp, False, res) []

data Pending = LossPending Int | ClearPending Int deriving (Eq, Ord, Read, Show)

finish :: Pending -> IGameState -> Results -> Results
finish pending igs result = case pending of
	LossPending pu -> result { lossDistribution = MS.singleton (pu - iPillsUsed igs) }
	ClearPending pu -> result { clearDistribution = MS.insert (pu - iPillsUsed igs) (clearDistribution result) }

processGame :: GameDetails -> IO Results
processGame (seed, steps, lk, _) = snd <$> fullReplay seed steps lk
	(\igs -> if iVirusesKilled igs == iOriginalVirusCount igs then (ClearPending (iPillsUsed igs), mempty) else (LossPending (iPillsUsed igs), mempty))
	\_step igs clearRes v@(pending, result) -> if 0 == clears (summarizeClearResults clearRes) || 0 /= iPillsUsed igs
		then v
		else (ClearPending (iPillsUsed igs), finish pending igs result)

ppResults :: Int -> Tree (FilePath, Results) -> IO ()
ppResults n (Node (fp, res) children) = do
	replicateM_ (n-1) (putStr "│ ")
	when (n>0) (putStr "├╴")
	putStrLn $ fp ++ ": clear = " ++ show (MS.toOccurList (clearDistribution res)) ++ "; loss = " ++ show (MS.toOccurList (lossDistribution res))
	traverse_ (ppResults (n+1)) children

instance Semigroup Results where
	Results l c np dne <> Results l' c' np' dne'
		= Results (l <> l') (c <> c') (np <> np') (dne <> dne')
instance Monoid Results where mempty = Results mempty mempty mempty mempty
