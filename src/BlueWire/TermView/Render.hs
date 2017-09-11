{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
module BlueWire.TermView.Render where

import BlueWire.TermView.Types
import qualified Brick as B
import qualified Brick.Widgets.List as B
import qualified Brick.Widgets.Core as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty.Attributes as B (defAttr, reverseVideo, withStyle)
import qualified Graphics.Vty.Input.Events as B
import BlueWire.Types
import BlueWire.Database.Opaleye.Schema (Profile')

-- | Render the application state
renderState :: BWVState -> [B.Widget ViewName]
renderState BWVState{..} = [wig] -- singleton list
    where
        -- The rendered widget
        wig = case view of
            -- The menu view! the main screen where the program
            -- starts, this lists possible views to switch to.
            -- Menu shouldn't appear here, but the case is accounted
            -- for anyway.
            MenuView{..} -> renderMenu menuStateList
            ServerView{..} ->
                case currentServer of
                    Just (sname, Nothing) -> undefined
            ConfigView{..} -> renderViewConfig conf

renderMenu :: B.List ViewName ViewName
           -> B.Widget ViewName
renderMenu menuList =
    let -- The single item render function, using the `selected`
        -- function to style selected elements.
        itemRender slct = {- B.hCenter . -} selected slct . \case
            Menu -> B.str "menu"
            Server -> B.str "select server"
            LocalConfig -> B.str "configure"
    in
    -- 4. center the smaller box
       B.center
    -- 3. apply a border to the box
     . B.borderWithLabel (B.str "|blue wire|")
    -- 2. limit the vertical
     . B.vLimit (length menuList)
    -- 1. limit the horizontal
     . B.hLimit 25
    -- Render the list, before we handle styles
     $ B.renderList itemRender True menuList

renderConfigItem :: ConfigItem -> B.Widget ViewName
renderConfigItem = \case
    BoolOpt name val -> B.str name B.<+> B.str (show val)
    StringOpt name val -> B.str name B.<+> B.str (show val)
    IntOpt name val -> B.str name B.<+> B.str (show val)

renderViewConfig :: ViewConfig -> B.Widget ViewName
renderViewConfig = undefined

renderServerSelect :: B.List ViewName (String, [String]) -> B.Widget ViewName
renderServerSelect = undefined

renderProfile :: String -> Profile' -> B.Widget ViewName
renderProfile profileName profile = undefined

selected :: Bool -> B.Widget n -> B.Widget n
selected True w = B.withAttr "inverted" w
selected _ w = w
