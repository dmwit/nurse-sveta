import Control.Applicative
import Control.Monad
import Data.ByteString.Builder (Builder)
import Dr.Mario.Model hiding (pp)
import Dr.Mario.Protocol.Raw
import System.Environment
import System.IO
import System.Random.MWC
import Text.Printf
import qualified Data.ByteString.Builder as B

render :: Board -> [(Color, Color)] -> Builder
render b ps = snd (pp b) <> newline <> foldMap renderPill ps where
	newline = B.char8 '\n'
	renderPill (l,r) = renderColor l <> renderColor r <> newline
	renderColor Blue   = B.char8 'b'
	renderColor Red    = B.char8 'r'
	renderColor Yellow = B.char8 'y'

main :: IO ()
main = do
	[dir, n_] <- getArgs
	n <- readIO n_
	gen <- createSystemRandom
	let word16 = uniformR (2, maxBound) gen
	    color = decodeColor <$> word16
	forM_ [0..20] $ \level -> forM_ [1..n :: Int] $ \i -> do
		seed <- word16
		ps <- replicateM 256 (liftA2 (,) color color)
		withFile (printf "%s/lev-%d-no-%d.in" dir level i) WriteMode $ \h ->
			B.hPutBuilder h (render (randomBoard seed level) ps)
