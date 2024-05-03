module Main where

import Control.Exception
import Nurse.Sveta.Files
import System.IO.Error

main :: IO ()
main = do
	dir <- nsDataDir
	catch (removeDirectoryRecursive dir) $ \e -> if isDoesNotExistError e
		then pure ()
		else throwIO e
