import System.Random.MWC
import qualified Dr.Mario.Model as M
import qualified Dr.Mario.Sveta as S

main :: IO ()
main = do
	gen <- createSystemRandom
	level <- uniformR (0,20) gen
	board <- flip M.randomBoard level <$> uniformR (2, maxBound) gen
	c1 <- M.decodeColor <$> uniformR (2, maxBound) gen
	c2 <- M.decodeColor <$> uniformR (2, maxBound) gen
	putStr . M.pp $ board
	putStr . S.pp $ S.unsafeApproxReachable
		board
		(M.Pill (M.PillContent M.Horizontal c1 c2) (M.Position 3 15))
