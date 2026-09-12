-- | The credentials the player needs to reach a Navidrome server, and the
-- shape of the plain config file they live in between runs.
--
-- The file holds one @field=value@ line per credential, the password among
-- them in the clear — no keyring, no encryption. Only two characters are ever
-- written differently from how they were given: a backslash and a newline,
-- escaped as @\\\\@ and @\\n@, so that any password whatsoever survives the
-- round trip through a line-oriented file.
--
-- Where that file is, and reading and writing it, is the shell's; what stands
-- in it is here.
module Havidrome.Credentials
  ( -- * Credentials
    Credentials (..)

    -- * The file's contents
  , render
  , parse
  , Fault (..)
  , explain
  ) where

import Data.Text (Text)
import Data.Text qualified as Text

-- | Everything needed to talk to one Navidrome account.
data Credentials = Credentials
  { server :: Text
  -- ^ The server's URL, as the user typed it.
  , username :: Text
  -- ^ The account on that server.
  , password :: Text
  -- ^ That account's password, kept and stored as it was given.
  }
  deriving stock (Eq)

-- | Shows the server and the username, and never the password: a credential
-- record that ends up in an error message must not carry the password with
-- it, however plainly the file itself stores it.
instance Show Credentials where
  showsPrec d credentials =
    showParen (d > 10) $
      showString "Credentials "
        . showsPrec 11 credentials.server
        . showString " "
        . showsPrec 11 credentials.username
        . showString " <password>"

-- | Why a config file could not be read as credentials.
data Fault
  = -- | The file is there but could not be read at all: no permission, not
    -- UTF-8, and so on. Carries the reason as reported.
    NotAccessible Text
  | -- | A line that is not @field=value@ for a field of t'Credentials'.
    BadLine Text
  | -- | A field the file never sets.
    MissingField Text
  | -- | A field the file sets more than once.
    RepeatedField Text
  deriving stock (Eq, Show)

-- | Why the credentials that were stored cannot be used, in the line the
-- player says before it stops. The fault stands in that line as it is
-- written here, which is what the player has always said; this is the only
-- place a 'Fault' becomes text.
explain :: Fault -> Text
explain fault =
  "the stored credentials could not be read: " <> Text.pack (show fault)

-- | The contents of a config file holding these credentials.
render :: Credentials -> Text
render credentials =
  Text.unlines
    [ field <> "=" <> escape (value credentials)
    | (field, value) <- fields
    ]

-- | Reads the contents of a config file back. Every way a file can fail to
-- be credentials is a 'Fault', never an exception.
parse :: Text -> Either Fault Credentials
parse text = do
  assigned <- traverse assignment (filter (not . Text.null) (Text.lines text))
  let only field = case [value | (field', value) <- assigned, field' == field] of
        [value] -> Right value
        [] -> Left (MissingField field)
        _ -> Left (RepeatedField field)
  Credentials <$> only serverField <*> only usernameField <*> only passwordField

-- | One @field=value@ line, with the value unescaped.
assignment :: Text -> Either Fault (Text, Text)
assignment line = maybe (Left (BadLine line)) Right $ do
  let (field, rest) = Text.breakOn "=" line
  escaped <- Text.stripPrefix "=" rest
  value <- unescape escaped
  if field `elem` map fst fields then Just (field, value) else Nothing

fields :: [(Text, Credentials -> Text)]
fields =
  [ (serverField, (.server))
  , (usernameField, (.username))
  , (passwordField, (.password))
  ]

serverField, usernameField, passwordField :: Text
serverField = "server"
usernameField = "username"
passwordField = "password"

-- | Hides from a line-oriented file the two characters that would confuse it.
escape :: Text -> Text
escape = Text.concatMap $ \character -> case character of
  '\\' -> "\\\\"
  '\n' -> "\\n"
  _ -> Text.singleton character

-- | The inverse of 'escape'; 'Nothing' for an escape that 'escape'
-- never writes.
unescape :: Text -> Maybe Text
unescape = fmap Text.pack . go . Text.unpack
  where
    go [] = Just []
    go ('\\' : character : rest) = case character of
      '\\' -> ('\\' :) <$> go rest
      'n' -> ('\n' :) <$> go rest
      _ -> Nothing
    go ['\\'] = Nothing
    go (character : rest) = (character :) <$> go rest
