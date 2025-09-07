module Main where

import Nurse.Sveta.Files
import Nurse.Sveta.Util

main :: IO ()
main = do
	dir <- nsDataDir
	catch (removeDirectoryRecursive dir) $ \e -> if isDoesNotExistError e
		then pure ()
		else throwIO e
