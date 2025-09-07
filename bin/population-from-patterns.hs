import Ms.Mendel

import qualified Data.Vector as V

main :: IO ()
main = do
	mmc <- loadConfiguration
	patterns <- loadFromConfiguration "patterns"
	dir <- basedir XdgData
	population <- replicateM (mmcInitialPopulation mmc) (iFromPatterns (mmcGeneMirroring mmc) patterns)
	savePopulation dir (V.fromList population) 0
