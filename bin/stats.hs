{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Csv
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Vector (Vector)
import Dr.Mario.Model
import Parser
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

main :: IO ()
main = do
	progName <- getProgName
	args <- getArgs
	root <- case args of
		[dir] -> pure dir
		_ -> die (usage progName)
	gather root >>= LC8.putStr . encodeDefaultOrderedByName

usage :: String -> String
usage progName = ""
	++ "USAGE: " ++ progName ++ " DIRECTORY\n"
	++ "Gather some statistics about game records stored in the given directory. They\n"
	++ "will be printed in CSV format to stdout."

data GameInformation = GameInformation
	{ directory :: FilePath
	, file :: FilePath
	, startingViruses :: Int
	, endingViruses :: Int
	, pillsUsed :: Int
	, ending :: Ending
	, finalJunk :: ColorMap Int
	} deriving (Eq, Ord, Read, Show)

gather :: FilePath -> IO [GameInformation]
gather = go "" where
	go dir file = goDirectory <|> goFile <|> complain where
		goDirectory = do
			let dir' = dir </> file
			ps <- listDirectory dir'
			concat <$> traverse (go dir') ps
		goFile = readGameRecord (dir </> file) >>= summarize dir file
		complain = [] <$ hPutStrLn stderr ("Skipping unparseable game record in " ++ (dir </> file))

summarize :: FilePath -> FilePath -> (Board, [Pill], Ending) -> IO [GameInformation]
summarize dir file (b, ps, e) = case boardStatesFromPills b ps >>= finalBoard of
	Left (b,p) -> fail ("Cannot place pill " ++ show p ++ " on board " ++ show b)
	Right b' -> pure . pure $ GameInformation
		{ directory = dir
		, file = file
		, startingViruses = countViruses b
		, endingViruses = countViruses b'
		, pillsUsed = length ps
		, ending = e
		, finalJunk = countJunk b'
		}

finalBoard :: [BoardState] -> Either (Board, Pill) Board
finalBoard bs = case last bs of
	BoardState { board = b, placement = Just p } -> case place b p of
		Just (_, b') -> Right b'
		Nothing -> Left (b, p)
	s -> Right (board s)

countViruses :: Board -> Int
countViruses = getSum . ofoldMap (countMaybe Virus . shape)

countJunk :: Board -> ColorMap Int
countJunk = fmap getSum . ofoldMap go where
	go = liftA3 colorMap (countMaybe Red) (countMaybe Yellow) (countMaybe Blue) . color

countMaybe :: Eq a => a -> Maybe a -> Sum Int
countMaybe needle (Just haystack) | needle == haystack = 1
countMaybe _ _ = 0

instance ToNamedRecord GameInformation where
	toNamedRecord gi = HM.fromList . tail $ [undefined
		, "directory" .-> UTF8.fromString (directory gi)
		, "file" .-> UTF8.fromString (file gi)
		, "starting-virus-count" .-> bshow (startingViruses gi)
		, "cleared-virus-count" .-> bshow (startingViruses gi - endingViruses gi)
		, "uncleared-virus-count" .-> bshow (endingViruses gi)
		, "pill-count" .-> bshow (pillsUsed gi)
		, "ending" .-> bshow (ending gi)
		, "uncleared-junk" .-> bshow (sum (finalJunk gi))
		, "uncleared-red-junk" .-> bshow (junk Red)
		, "uncleared-yellow-junk" .-> bshow (junk Yellow)
		, "uncleared-blue-junk" .-> bshow (junk Blue)
		] where ColorMap junk = finalJunk gi

instance DefaultOrdered GameInformation where
	headerOrder _ = V.fromList . map C8.pack . tail $ [undefined
		, "directory"
		, "file"
		, "starting-virus-count"
		, "cleared-virus-count"
		, "uncleared-virus-count"
		, "pill-count"
		, "ending"
		, "uncleared-junk"
		, "uncleared-red-junk"
		, "uncleared-yellow-junk"
		, "uncleared-blue-junk"
		]

(.->) :: String -> ByteString -> (ByteString, ByteString)
s .-> bs = (C8.pack s, bs)

bshow :: Show a => a -> ByteString
bshow = C8.pack . show

newtype ColorMap a = ColorMap (Color -> a) deriving (Functor, Applicative, Monad, Semigroup, Monoid)

colors :: [Color]
colors = [Red, Yellow, Blue]

colorMap :: a -> a -> a -> ColorMap a
colorMap r y b = ColorMap $ \c -> case c of
	Red -> r
	Yellow -> y
	Blue -> b

instance Eq a => Eq (ColorMap a) where ColorMap f == ColorMap g = map f colors == map g colors
instance Ord a => Ord (ColorMap a) where compare (ColorMap f) (ColorMap g) = compare (map f colors) (map g colors)
instance Show a => Show (ColorMap a) where show (ColorMap f) = show [(c, f c) | c <- colors]
instance Read a => Read (ColorMap a) where
	readsPrec n s = readsPrec n s >>= fromList where
		fromList ([(Red, r), (Yellow, y), (Blue, b)], s) = pure . flip (,) s $ colorMap r y b
		fromList _ = []
instance Traversable ColorMap where
	traverse f (ColorMap g) = pure colorMap
		<*> f (g Red)
		<*> f (g Yellow)
		<*> f (g Blue)
instance Foldable ColorMap where foldMap f (ColorMap g) = f (g Red) <> f (g Yellow) <> f (g Blue)
