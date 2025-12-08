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

		gPatternGroups <- traverse (newGIRef . repurpose @_ @_ @Browsing (pgaDefaultReplication patternGroups)) (pgaPatternGroups patternGroups)
		pgView <- createListView ListViewParameters
			{ lvpContents = gPatternGroups
			, lvpSetupWidget = new Label []
			, lvpBindWidget = \gPatternGroup lbl ->
				set lbl [#label :=> pgbDescription <$> readGIRef gPatternGroup]
			, lvpSelection = \i gPatternGroup -> print i >> readGIRef gPatternGroup >>= print
			}

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

data ListViewParameters t a w = ListViewParameters
	{ lvpContents :: t a
	, lvpSetupWidget :: IO w
	, lvpBindWidget :: a -> w -> IO ()
	, lvpSelection :: Word32 -> a -> IO ()
	}

-- see https://discourse.haskell.org/t/haskell-gi-how-do-i-create-a-gtk-listview/11059/7
createListView :: forall a w t. (Traversable t, IsObject a, IsWidget w) => ListViewParameters t a w -> IO ListView
createListView lvp = do
	store <- new ListStore []
	model <- new SingleSelection [#model := store]
	factory <- new SignalListItemFactory []

	traverse_ (#append store) (lvpContents lvp)

	on factory #setup \objItem -> do
		Just item <- castTo ListItem objItem
		widget <- lvpSetupWidget lvp
		#setChild item (Just widget)
	on factory #bind \objItem -> do
		Just item <- castTo ListItem objItem
		Just objRow <- #getItem item
		Just objWidget <- #getChild item
		Just row <- castTo @_ @a coerce objRow
		Just widget <- castTo @_ @w coerce objWidget
		lvpBindWidget lvp row widget
	on model #selectionChanged \_ _ -> do
		i <- get model #selected
		Just objRow <- get model #selectedItem
		Just obj <- castTo coerce objRow
		lvpSelection lvp i obj

	new ListView [#model := model, #factory := factory]
