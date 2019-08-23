{-# LANGUAGE FlexibleInstances #-}
module Dr.Mario.Sveta.PP where

import Data.List
import Data.Hashable
import Data.Ord
import Data.HashPSQ (HashPSQ)
import Data.HashSet (HashSet)
import Dr.Mario.Model hiding (pp)
import Text.Printf
import qualified Data.HashPSQ as Q
import qualified Data.HashSet as HS
import qualified Dr.Mario.Model as M

putPP :: PP a => a -> IO ()
putPP = putStr . pp

putPPLn :: PP a => a -> IO ()
putPPLn = putStrLn . pp

class PP a where pp :: a -> String
instance PP Double where pp = printf "%01.2f"
instance PP Position where pp (Position x y) = "(" ++ show x ++ "," ++ show y ++ ")"
instance PP Direction where
	pp d | d == left = "←"
	     | d == right = "→"
	     | d == down = "↓"
instance PP Rotation where
	pp Clockwise = "↻"
	pp Counterclockwise = "↺"
instance PP Orientation where
	pp Vertical = "↕"
	pp Horizontal = "↔"
instance PP Color where
	pp Blue = "b"
	pp Red = "r"
	pp Yellow = "y"
instance PP PillContent where pp (PillContent o c c') = pp o ++ pp c ++ pp c'
instance PP Pill where pp (Pill c pos) = pp c ++ "@" ++ pp pos
instance PP Board where pp = M.pp
instance (PP a, PP b) => PP (a, b) where pp (a, b) = pp a ++ ": " ++ pp b
instance PP [Pill] where pp = unlines . map pp . sort
instance PP (HashSet Pill) where pp = pp . HS.toList
instance PP a => PP (Down a) where pp (Down a) = pp a
instance (Hashable k, Ord k, Ord p, PP k, PP p, PP v) => PP (HashPSQ k p v) where
	pp q = case Q.minView q of
		Nothing -> ""
		Just (k, p, v, q') -> "(" ++ pp p ++ ") " ++ pp k ++ " => " ++ pp v ++ "\n" ++ pp q'
