import Control.Monad
import Data.Aeson
import Nurse.Sveta.Files
import Nurse.Sveta.Genome
import System.Directory
import System.Environment

import qualified Data.Vector as V

main :: IO ()
main = do
	configDir <- getXdgDirectory XdgConfig "ms-mendel"
	dir <- getXdgDirectory XdgData "ms-mendel"
	patterns <- eitherDecodeFileStrict (configDir </> "patterns.json") >>= either fail pure
	[n_] <- getArgs
	n <- readIO n_
	population <- replicateM n (iFromPatterns False patterns)
	encodeFile (dir </> "0.json") (RecordOfVectors (iSpec <$> V.fromList population))
	encodeFile @Int (dir </> "latest.json") 0
