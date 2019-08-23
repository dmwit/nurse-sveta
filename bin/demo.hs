import Dr.Mario.Sveta
import Dr.Mario.Sveta.MCTS
import Dr.Mario.Sveta.PP
import System.Random.MWC
import System.IO
import qualified Dr.Mario.Model as M

main :: IO ()
main = do
	gen <- createSystemRandom
	level <- uniformR (0,20) gen
	board <- flip M.randomBoard level <$> uniformR (2, maxBound) gen
	c1 <- M.decodeColor <$> uniformR (2, maxBound) gen
	c2 <- M.decodeColor <$> uniformR (2, maxBound) gen
	hSetBuffering stdin NoBuffering
	putStr "\ESC[2J"
	let params = dmParameters gen board
	    go t = do
	    	putStr "\ESC[;H"
	    	putPP board
	    	putPP (statistics <$> children t)
	    	getChar
	    	mcts params t >>= go
	emptyTree params >>= go
