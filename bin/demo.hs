import Dr.Mario.Model hiding (pp)
import Dr.Mario.Sveta

main :: IO ()
main = putStr . pp $ reachable
	(emptyBoard 8 16)
	14
	(Pill (PillContent Horizontal Red Blue) (Position 3 15))
	Checking
