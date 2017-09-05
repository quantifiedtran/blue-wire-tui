module BlueWire.TermView.Types where

import qualified Brick as B
import qualified Brick.Widgets.List as B
import qualified Brick.Widgets.Core as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty.Attributes as B (defAttr, reverseVideo, withStyle)
import qualified Graphics.Vty.Input.Events as B
import BlueWire.Types
import BlueWire.Database.Opaleye.Schema (Profile')


-- | The views in the TUI
data ViewName
    = Menu
    | Server
    | LocalConfig
    deriving (Eq, Ord, Show)

type BWEvent = ()

-- | the configuration of the program
data ViewConfig = ViewConfig
    { shouldAutoloadServer :: Bool
    , autoloadServer :: Maybe (String, Maybe String)
    , knownServers :: [(String, [String])]
    } deriving (Eq, Ord, Show)

defaultVC = ViewConfig False Nothing []

data ConfigItem
    = BoolOpt String Bool
    | StringOpt String String
    | IntOpt String Int
    deriving (Show)

data BWVSum
    = ServerView
        { currentServer :: Maybe (String, Maybe Profile')
        , serverStateList :: B.List ViewName (String, [String])
        }
    | ConfigView { confStateList :: B.List ViewName ConfigItem}
    | MenuView { menuStateList :: B.List ViewName ViewName }
    deriving (Show)

data BWVState = BWVState
    { view :: BWVSum
    , conf :: ViewConfig
    , confFile :: FilePath
    } deriving (Show)

