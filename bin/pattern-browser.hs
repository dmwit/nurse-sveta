module Main where

import GI.Gtk hiding (ListStore(..))
import GI.Gio.Objects.ListStore
import GI.GObject.Objects.Object
import Ms.Mendel hiding (get)
import Nurse.Sveta.Cairo
import Nurse.Sveta.Widget

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

-- ╭╴w╶──────╮
-- │╭╴top╶──╮│
-- ││╭╴gen╶╮││
-- ││╰─────╯││
-- ││╭╴ivd╶╮││
-- ││╰─────╯││
-- ││╭╴cnv╶╮││
-- ││╰─────╯││
-- ││╭╴hgv╶╮││
-- ││╰─────╯││
-- │╰───────╯│
-- ╰─────────╯
main :: IO ()
main = do
	torchPlusGtkFix
	cfgDir <- basedir XdgConfig
	dataDir <- basedir XdgData
	app <- new Application []
	patternGroups <- loadPatterns_ cfgDir
	on app #activate do
		top <- new Box [#orientation := OrientationVertical, #spacing := 4]

		pgView <- createListView =<< traverse (repurposeIO @_ @_ @GI (pgaDefaultReplication patternGroups)) (pgaPatternGroups patternGroups)
		#append top pgView

		w <- new Window $ tail [ignored
			, #title := "Ms. Mendel Pattern Browser"
			, #application := app
			, #child := top
			, #defaultWidth := 1500
			, #defaultHeight := 1000
			, On #closeRequest (performGC >> #quit app >> pure True)
			]

		#show w
	args <- getArgs
	() <$ #run app (Just args)

-- see https://discourse.haskell.org/t/haskell-gi-how-do-i-create-a-gtk-listview/11059/7
createListView :: forall a t. (Traversable t, IsObject a) => t a -> IO ListView
createListView as = do
	store <- new ListStore []
	model <- new SingleSelection [#model := store]
	factory <- new SignalListItemFactory []

	traverse_ (#append store) as

	on factory #setup \objItem -> do
		Just item <- castTo ListItem objItem
		label <- new Label []
		#setChild item (Just label)
	on factory #bind \objItem -> do
		Just item <- castTo ListItem objItem
		Just objRow <- #getItem item
		Just objWidget <- #getChild item
		Just row <- castTo @_ @a coerce objRow
		Just widget <- castTo Label objWidget
		set widget [#label := "hello there"]

	new ListView [#model := model, #factory := factory]
