import Dr.Mario.Sveta
import Dr.Mario.Sveta.MCTS
import Dr.Mario.Sveta.PP
import System.Random.MWC
import System.IO
import qualified Dr.Mario.Model as M

countNodes :: MCTree stats score move -> Int
countNodes = succ . sum . fmap countNodes . children

main :: IO ()
main = do
	gen <- createSystemRandom
	level <- uniformR (0,0) gen
	board <- flip M.randomBoard level <$> uniformR (2, maxBound) gen
	c1 <- M.decodeColor <$> uniformR (2, maxBound) gen
	c2 <- M.decodeColor <$> uniformR (2, maxBound) gen
	hSetBuffering stdin NoBuffering
	putStr "\ESC[2J"
	params <- dmReroot (dmParameters gen board) (ChanceMove c1 c2)
	let mctsNTimes 0 t = pure t
	    mctsNTimes n t = mcts params t >>= mctsNTimes (n-1)
	    go t = do
	    	putStr "\ESC[;H"
	    	putPP board
	    	putPP (statistics <$> children t)
	    	putStr "Total nodes: "
	    	print (countNodes t)
	    	getChar
	    	mctsNTimes 10000 t >>= go
	emptyTree params >>= go
