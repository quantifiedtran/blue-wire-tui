{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
module BlueWire.TermView.Logic where

import qualified Brick as B
import qualified Brick.Widgets.List as B
import qualified Brick.Widgets.Core as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty.Attributes as B (defAttr, reverseVideo, withStyle)
import qualified Graphics.Vty.Input.Events as B

import BlueWire.TermView.Types

switchView :: ViewName -> BWVState -> BWVState
switchView switchto state@BWVState{..} =
    case switchto of
        Menu -> state{ view = MenuView (menuList Menu) }
        Server ->
            if shouldAutoloadServer conf
               then state{ view = ServerView Nothing {- (autoloadServer conf) -} (B.list Server [] 1) }
               else state{ view = ServerView Nothing (B.list Server [] 1)}
        LocalConfig -> state { view = ConfigView (confList conf LocalConfig) }


menuList :: Ord n => n -> B.List n ViewName
menuList n =
    let list = [Server, LocalConfig]
    in B.list n list 1

confList :: Ord n => ViewConfig -> n -> B.List n ConfigItem
confList conf@ViewConfig{..} name =
    let renderServer :: Maybe (String, Maybe String) -> String
        renderServer = \case
            Nothing -> ""
            Just (server, Nothing) -> server
            Just (server, Just profile) -> server ++ ", " ++ profile
        confTree =
            [ BoolOpt "Should Autoload Server" shouldAutoloadServer
            , StringOpt "Server To Autoload" (renderServer autoloadServer)
            ]
    in B.list name confTree 1

serverList :: Ord n => [(String, [String])] -> n -> List n (String, [String])
serverList servers =
