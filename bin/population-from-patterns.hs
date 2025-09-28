import Ms.Mendel

import qualified Data.Vector as V

main :: IO ()
main = do
	mmc <- loadConfiguration
	patterns <- loadFromConfiguration "patterns"
	dir <- basedir XdgData
	rng <- createSystemRandom

	iBase <- iFromPatterns (mmcGeneMirroring mmc) patterns
	for_ iBase \g -> for_ [0..gSize g-1] \pat -> gSetPatternScore g pat 0
	population <- V.replicateM (mmcInitialPopulation mmc) do
		i <- iClone iBase
		let patternChoices = V.fromList [(g, pat) | g <- toList i, pat <- [0..gSize g-1]]
		i <$ replicateM_ (mmcInitialSinglePatternScoreAdjustments mmc) do
			(g, pat) <- uniformV' rng patternChoices
			score <- uniformFloat01M rng
			gSetPatternScore g pat (2*score-1)

	savePopulation dir population 0
