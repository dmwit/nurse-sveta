module Main where

import Control.Exception
import System.Directory
import System.Environment.XDG.BaseDir
import System.IO.Error

main :: IO ()
main = do
	dir <- getUserDataDir "nurse-sveta"
	catch (removeDirectoryRecursive dir) $ \e -> if isDoesNotExistError e
		then pure ()
		else throwIO e
