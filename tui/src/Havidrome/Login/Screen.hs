-- | The login screen: the form drawn on a terminal, the keys that fill it in,
-- and the server and config file a submit really reaches.
--
-- Everything the screen does with what was typed is 'Havidrome.Login''s; this
-- is the terminal it is typed on.
module Havidrome.Login.Screen
  ( -- * The screen
    Screen (..)
  , Name (..)
  , login
  , application

    -- * Where a submit really goes
  , navidrome

    -- * What it looks like
  , draw
  , theme
  ) where

import Brick
  ( App (..)
  , AttrMap
  , AttrName
  , BrickEvent (VtyEvent)
  , EventM
  , Padding (Max)
  , Widget
  , attrMap
  , attrName
  , emptyWidget
  , fill
  , halt
  , neverShowCursor
  , padRight
  , txt
  , vBox
  , withAttr
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Data.Text as T (Text)
import Graphics.Vty qualified as Vty
import Havidrome.Credentials qualified as Credentials
import Havidrome.Credentials.Store (HasStore (getStore), Store (save))
import Havidrome.Key.Vty (pressed)
import Havidrome.Login
  ( Ending (Abandoned, Entered)
  , Entry (..)
  , Field
  , Form (..)
  , blank
  , command
  , fields
  , labelled
  , masked
  , step
  , value
  )
import Havidrome.Margin (margined)
import Havidrome.Subsonic (HasSubsonic (getSubsonic), Subsonic (accepts))
import Havidrome.Terminal (HasTerminal (getTerminal), onTerminal)

-- | The real entry: the server the typed URL names, asked through the player's
-- own calls, and the config file the accepted credentials are stored in.
navidrome :: (HasStore env, HasSubsonic env) => env -> Entry IO
navidrome env =
  Entry
    { accepts = (getSubsonic env).accepts
    , keeps = (getStore env).save
    }

-- | The name brick knows the screen by. There is one thing on it, so there is
-- one name.
data Name = Prompt
  deriving stock (Eq, Ord, Show)

-- | Everything the screen is: the form being filled in, and the ending it
-- reached, once it has reached one.
data Screen = Screen
  { form :: Form
  , ending :: Maybe Ending
  }
  deriving stock (Eq, Show)

-- | Asks for credentials, and hands back the ones a server took — stored by
-- then — or nothing at all when the player was left.
login :: (HasTerminal env) => env -> Entry IO -> IO (Maybe Credentials.Credentials)
login env entry = do
  final <-
    onTerminal (getTerminal env) Nothing (application entry) (Screen blank Nothing)
  pure $ case final.ending of
    Just (Entered credentials) -> Just credentials
    Just Abandoned -> Nothing
    Nothing -> Nothing

-- | The screen itself, up until it is submitted successfully or left.
application :: Entry IO -> App Screen e Name
application entry =
  App
    { appDraw = draw . (.form)
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handle entry
    , appStartEvent = pure ()
    , appAttrMap = const theme
    }

handle :: Entry IO -> BrickEvent Name e -> EventM Name Screen ()
handle entry = \case
  VtyEvent (Vty.EvKey key modifiers) ->
    case pressed key modifiers >>= uncurry command of
      Nothing -> pure ()
      Just instruction -> do
        screen <- get
        stepped <- liftIO (step entry instruction screen.form)
        case stepped of
          Left ended -> put screen {ending = Just ended} >> halt
          Right typed -> put screen {form = typed}
  _ -> pure ()

-- | The whole screen, inside the margin every screen has: the player's name,
-- the three fields under it with the focused one standing out, and the bottom
-- strip when a server has said something.
draw :: Form -> [Widget Name]
draw form =
  [ margined . vBox $
      [ withAttr titleAttribute (line "havidrome")
      , line " "
      ]
        <> fmap field fields
        <> [ fill ' '
           , maybe emptyWidget (withAttr troubleAttribute . line) form.trouble
           ]
  ]
  where
    field :: Field -> Widget Name
    field which =
      standOut which (line (labelled which <> masked which (value which form)))
    standOut which = if form.focus == which then withAttr focusedAttribute else id

-- | A row of text across the full width, so that marking one covers the line
-- and not just its letters.
line :: Text -> Widget Name
line = padRight Max . txt

-- | The focused field is the row in reverse video, as the selected row is when
-- browsing; the title is bold and the bottom strip red.
theme :: AttrMap
theme =
  attrMap
    Vty.defAttr
    [ (focusedAttribute, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
    , (titleAttribute, Vty.defAttr `Vty.withStyle` Vty.bold)
    , (troubleAttribute, Vty.defAttr `Vty.withForeColor` Vty.red)
    ]

titleAttribute, focusedAttribute, troubleAttribute :: AttrName
titleAttribute = attrName "title"
focusedAttribute = attrName "focused"
troubleAttribute = attrName "trouble"
