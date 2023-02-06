module Main where

import System.Directory
import System.Environment.XDG.BaseDir

main :: IO ()
main = getUserDataDir "nurse-sveta" >>= removeDirectoryRecursive
