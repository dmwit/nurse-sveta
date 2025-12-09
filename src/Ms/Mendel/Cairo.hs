module Ms.Mendel.Cairo where

import GI.Cairo.Render
import Ms.Mendel.CXX.Cooked
import Ms.Mendel.Population
import Nurse.Sveta.Cairo
import Nurse.Sveta.Util

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified GI.Cairo.Render as C
import qualified Ms.Mendel.Population as Mendel
import qualified Nurse.Sveta.Cairo as NC

pcRender :: PatternCell -> Render ()
pcRender pc = do
	C.save
	when (pc == pcAnything) do
		setSourceRGBA 0 1 0 0.2
		rectangle 0 0 1 1
		fill
	when (pc == pcNothing) do
		setSourceRGBA 1 0 0 0.2
		rectangle 0 0 1 1
		fill
	scale 0.2 0.2
	for_ [(Blue, 0), (Red, 1), (Yellow, 2)] \(c, x) -> allowed
		(NonSentinel (Left c))
		(NC.shape x 2 (setColor c) Disconnected)
	for_ [(Virus, -0.5), (Disconnected, 0.5), (West, 1.5), (East, 2.5)] \(s, x) -> allowed
		(NonSentinel (Right s))
		(NC.shape x 1 neutral s)
	allowed EmptySentinel (fitText 1.5 1 1 1 "ε")
	allowed OutOfBoundsSentinel (fitText 2.5 1 1 1 "x")
	C.restore
	where
	allowed ws act = do
		pushGroup
		act
		popGroupToSource
		paintWithAlpha if ws `S.member` pcAllowed pc then 1 else 0.1

ptbRender :: Mendel.PatternTemplate Browsing -> Render ()
ptbRender ptb =
	for_ [0..csWidth (ptbConvolutionSize ptb)-1] \x ->
	for_ [0..csHeight (ptbConvolutionSize ptb)-1] \y -> do
		C.save
		translate (fromIntegral x) (fromIntegral y)
		pcRender (ptbCells ptb V.! y V.! x)
		C.restore
