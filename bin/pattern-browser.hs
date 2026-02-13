module Main where

import GI.Gtk hiding (ListStore(..))
import GI.Gio.Objects.ListStore
import GI.GObject.Objects.Object
import Ms.Mendel hiding (get)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

-- ╭╴w╶─────────────────╮
-- │╭╴top╶─────────────╮│
-- ││╭╴namesView╶─────╮││
-- ││╰────────────────╯││
-- ││╭╴renderingsView╶╮││
-- ││╰────────────────╯││
-- │╰──────────────────╯│
-- ╰────────────────────╯
main :: IO ()
main = do
	torchPlusGtkFix
	cfgDir <- basedir XdgConfig
	dataDir <- basedir XdgData
	app <- new Application []
	patternGroups <- loadPatterns_ cfgDir
	on app #activate do
		top <- new Box [#orientation := OrientationVertical, #spacing := 4]

		renderingsView <- createListView @Widget ListViewParameters
			{ lvpContents = []
			, lvpSetupWidget = new Box []
			, lvpBindWidget = #append
			, lvpUnbindWidget = #remove
			, lvpSelection = def
			}
		set renderingsView [#showSeparators := True]

		gPatternGroups <- traverse newGIRef $ pgbPatternGroups patternGroups
		namesView <- createListView ListViewParameters
			{ lvpContents = gPatternGroups
			, lvpSetupWidget = new Label []
			, lvpBindWidget = \lbl gPatternGroup ->
				set lbl [#label :=> pgbDescription <$> readGIRef gPatternGroup]
			, lvpUnbindWidget = def
			, lvpSelection = \i gPatternGroup -> do
				pgb <- readGIRef gPatternGroup
				store <- new ListStore []
				traverse_ (#append store <=< pgWidget <=< repurposeIO') (pgbPatterns pgb)
				set renderingsView [#model :=> new NoSelection [#model := store]]
			}

		#append top namesView
		#append top renderingsView

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
	, lvpBindWidget :: w -> a -> IO ()
	, lvpUnbindWidget :: w -> a -> IO ()
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
		Just row <- castTo coerce objRow
		Just widget <- castTo coerce objWidget
		lvpBindWidget lvp widget row
	on factory #unbind \objItem -> do
		Just item <- castTo ListItem objItem
		Just objRow <- #getItem item
		Just objWidget <- #getChild item
		Just row <- castTo coerce objRow
		Just widget <- castTo coerce objWidget
		lvpUnbindWidget lvp widget row
	on model #selectionChanged \_ _ -> do
		i <- get model #selected
		Just objRow <- get model #selectedItem
		Just row <- castTo coerce objRow
		lvpSelection lvp i row

	new ListView [#model := model, #factory := factory]
