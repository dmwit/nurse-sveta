import System.Random.MWC
import qualified Dr.Mario.Model as M
import qualified Dr.Mario.Sveta as S

main :: IO ()
main = do
	gen <- createSystemRandom
	board <- flip M.randomBoard 4 <$> uniformR (1, maxBound) gen
	c1 <- M.decodeColor <$> uniformR (1, maxBound) gen
	c2 <- M.decodeColor <$> uniformR (1, maxBound) gen
	putStr . M.pp $ board
	putStr . S.pp $ S.reachable
		board
		14
		(M.Pill (M.PillContent M.Horizontal c1 c2) (M.Position 3 15))
		S.Checking
