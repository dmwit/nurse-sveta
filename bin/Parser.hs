module Parser (Ending(..), describeEnding, readGameRecord, BoardState(..), boardStatesFromPills) where

import Control.Applicative
import Data.Char
import Data.Foldable
import Dr.Mario.Model
import Dr.Mario.Protocol.Raw
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS

readGameRecord :: FilePath -> IO (Board, [Pill], Ending)
readGameRecord fp = do
	bs <- LBS.readFile fp
	case A.parse gameFormat bs of
		A.Fail bs ctxts err -> fail
			$  "Parsing of " ++ fp ++ " failed:\n"
			++ err ++ "\n"
			++ "Context:\n"
			++ unlines ctxts
			++ "First little bit of what was left:\n"
			++ show (LBS.take 60 bs)
		A.Done bs r
			| LBS.null bs -> pure r
			| otherwise -> fail
				$  "Leftover junk at end of " ++ fp ++ ", starting like this:\n"
				++ show (LBS.take 60 bs)

data Ending = End | Timeout | Stall deriving (Bounded, Enum, Eq, Ord, Read, Show)

describeEnding :: Ending -> String
describeEnding End = "The game ended normally."
describeEnding Timeout = "The AI timed out without finishing a single rollout from this position."
describeEnding Stall = "Stalemate: gameplay halted for placing too many pills without clearing a virus."

gameFormat :: A.Parser (Board, [Pill], Ending)
gameFormat = liftA3 (,,)
	(parseAndWarn <* newline)
	(some parseIgnoreAndWarn)
	(ending <* newline)
	where
	newline = A.word8 10
	ending = asum [e <$ (A.string . C8.pack . map toLower . show) e | e <- [minBound .. maxBound]]

parseIgnoreAndWarn :: Protocol a => A.Parser a
parseIgnoreAndWarn = parseAndWarn <* A.takeWhile (/= 10) <* A.word8 10

parseAndWarn :: Protocol a => A.Parser a
parseAndWarn = do
	(a, ws) <- parse
	case ws of
		[] -> pure ()
		_ -> fail (show ws)
	pure a

data BoardState = BoardState
	{ board :: Board
	, placement :: Maybe Pill
	, lookahead :: Maybe (Color, Color)
	} deriving (Eq, Ord, Read, Show)

boardStatesFromPills :: Board -> [Pill] -> Either (Board, Pill) [BoardState]
boardStatesFromPills b [] = Right [BoardState b Nothing Nothing]
boardStatesFromPills b (p:ps) = (BoardState b Nothing (Just (lookaheadFromPill p)):) <$> go b p ps where
	go b p [] = Right [BoardState b (Just p) Nothing]
	go b p (p':ps) = case place b p of
		Nothing -> Left (b, p)
		Just (_, b') -> (BoardState b (Just p) (Just (lookaheadFromPill p')):)
			<$> go b' p' ps

	lookaheadFromPill :: Pill -> (Color, Color)
	lookaheadFromPill p = (bottomLeftColor (content p), otherColor (content p))
