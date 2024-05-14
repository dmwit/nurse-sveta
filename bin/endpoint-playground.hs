import Control.Monad
import Data.Foldable
import Foreign.C
import System.IO

import qualified Data.Vector.Storable as V

import Nurse.Sveta.Torch
import Nurse.Sveta.Torch.Endpoint

main :: IO ()
main = do
	testAll cGameConstant hsGameConstant dumpGameConstant exGameConstant
	for_ exGameConstant \gc -> do
		putStr if evalGameConstant gc == evalCGameConstant (cGameConstant_ gc) then "✓" else "❌"
		putStrLn $ " " ++ show gc
	testDump cStructure dumpStructure exStructure
	testAll cEndpoint hsEndpoint dumpEndpoint exEndpointNoMasks

	for_ [minBound..maxBound] \ty -> do
		putStrLn $ "\nOutput leaf type: " ++ show ty
		net <- nextNetSample' True
			(STensor Positive [GCWidth, GCHeight])
			(STensor ty [GCWidth, GCHeight])
		let i = EFullTensor [GCWidth, GCHeight] (generate [2,8,16] \[n, i, c] -> fromIntegral n*0.2 + fromIntegral i*0.03 + fromIntegral c*0.005)
		o <- nextNetEvaluation' net i
		putStr "Input: "
		print i
		putStr "\nOutput: "
		print o
		-- TODO: test maskedtensor ground truths
		putStr "\nSelf-input loss: "
		nextNetDetailedLoss' net i i >>= print

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
exGameConstant = [GCColors, GCShapes, GCWidth, GCHeight, GCOrientations] ++ map GCMiscellaneous [0..10]

exStructure :: [Structure]
exStructure = [STensor ty [] | ty <- [minBound..maxBound]] ++ tail [undefined
	, STensor Unit [GCMiscellaneous 3]
	, STensor Unit [GCShapes]
	, SVector GCShapes (STensor Unit [])
	, SVector GCShapes (STensor Unit [GCMiscellaneous 1])
	, SDictionary []
	, SDictionary $ tail [undefined
		,("A", STensor Categorical [GCWidth, GCHeight])
		,("B", SVector GCColors (SVector GCShapes (SDictionary [("C", STensor Unit [])])))
		,("C", SDictionary [])
		]
	]

exEndpointNoMasks :: [Endpoint]
exEndpointNoMasks =
	[ EFullTensor [] (generate bs \[i] -> fromIntegral i)
	| bs <- [[0], [10]]
	] ++
	[ eFullTensor len dims
	| len <- [0, 3]
	, dims <- [[], [GCOrientations, GCColors]]
	] ++ tail [undefined
	, EDictionary $ tail [undefined
		, ("A", eFullTensor 3 [GCOrientations, GCColors])
		, ("B", eFullTensor 3 [GCColors, GCShapes])
		, ("C", EVector GCOrientations [eFullTensor 3 [], eFullTensor 3 []])
		]
	]
	where

eFullTensor :: Int -> [GameConstant] -> Endpoint
eFullTensor len dims = EFullTensor dims (tensorVals len dims)

tensorVals :: Int -> [GameConstant] -> StridedVector CFloat
tensorVals len dims = generate (len:evalGameConstants dims) (fromIntegral . foldl' (\n i -> 10*n+i) 9)
