import Control.Monad
import Data.Foldable
import Foreign.C
import System.IO

import qualified Data.Vector.Storable as V

import Nurse.Sveta.Torch.Endpoint

main :: IO ()
main = do
	testAll cGameConstant hsGameConstant dumpGameConstant exGameConstant
	for_ exGameConstant \gc -> do
		putStr if evalGameConstant gc == evalCGameConstant (cGameConstant_ gc) then "✓" else "❌"
		putStrLn $ " " ++ show gc
	testAll cDimensions hsDimensions dumpDimensions exDimensions
	testDump cStructure dumpStructure exStructure
	testAll cEndpoint hsEndpoint dumpEndpoint exEndpointNoMasks

testAll :: (Eq a, Show a) => (a -> IO b) -> (b -> IO a) -> (b -> IO ()) -> [a] -> IO ()
testAll c hs dump as = testDump c dump as >> testRoundtrip c hs as

testDump :: (a -> IO b) -> (b -> IO ()) -> [a] -> IO ()
testDump c dump = traverse_ \a -> c a >>= dump >> putStrLn ""

testRoundtrip :: (Eq a, Show a) => (a -> IO b) -> (b -> IO a) -> [a] -> IO ()
testRoundtrip c hs = traverse_ \a -> do
	a' <- c a >>= hs
	putStr if a == a' then "✓" else "❌"
	putStrLn $ " " ++ show a

exGameConstant :: [GameConstant]
exGameConstant = [minBound..maxBound]

exDimensions :: [Dimensions]
exDimensions = [Sum [], Product [], Sum [GCColors], Product [GCShapes], Sum [minBound..maxBound], Product [minBound..maxBound]]

exStructure :: [Structure]
exStructure = map SLeaf [minBound..maxBound] ++ tail [undefined
	, SHeterogeneous $ tail [undefined
		,("A", SMasked (SRectangle (Product [GCWidth, GCHeight]) (SLeaf Categorical)))
		,("B", SRectangle (Sum [GCColors, GCShapes]) (SHeterogeneous [("C", SLeaf Unit)]))
		,("C", SHeterogeneous [])
		]
	]

exEndpointNoMasks :: [Endpoint]
exEndpointNoMasks =
	[ ELeaf ty elems
	| ty <- [minBound..maxBound]
	, elems <- [V.empty, V.fromList [1..10]]
	] ++
	[ eRectangle ty dims len
	| ty <- [minBound..maxBound]
	, len <- [0, 3]
	, con <- [Sum, Product]
	, args <- [[], [GCOrientations, GCColors]]
	, let dims = con args
	] ++ tail [undefined
	, EHeterogeneous $ tail [undefined
		, ("A", eRectangle Categorical (Product [GCOrientations, GCColors]) 3)
		, ("B", eRectangle Unit (Sum [GCColors, GCShapes]) 3)
		]
	]
	where

eRectangle :: LeafType -> Dimensions -> Int -> Endpoint
eRectangle ty dims len = ERectangle ty dims (rectVals len dims)

rectVals :: Int -> Dimensions -> StridedVector CFloat
rectVals len dims = generate (len:evalDimensions dims) (fromIntegral . foldl' (\n i -> 10*n+i) 9)
