{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
module BlueWire.TermView where

import qualified Servant.Client as S
import qualified Brick as B
import qualified Brick.Widgets.List as B
import qualified Brick.Widgets.Core as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty.Attributes as B (defAttr, reverseVideo, withStyle)
import qualified Graphics.Vty.Input.Events as B
import BlueWire.Types
import BlueWire.Servant
import BlueWire.Database.Opaleye.Schema (Profile')
import System.IO
import BlueWire.TermView.Render
import BlueWire.TermView.Types
import BlueWire.TermView.Logic

bwviewer :: IO ()
bwviewer = do
    finalState <- B.defaultMain bwapp (BWVState (MenuView $ menuList Menu)
                                                (ViewConfig False Nothing [])
                                                "~/.bwvconf")
    return ()

handleEvents :: BWVState
             -> B.BrickEvent ViewName BWEvent
             -> B.EventM ViewName (B.Next BWVState)
handleEvents st@BWVState{..} ev =
    case view of
        menu@MenuView{..} -> case ev of
            B.VtyEvent (B.EvKey B.KEsc _) -> B.halt st
            B.VtyEvent (B.EvKey B.KEnter _) ->
                case B.listSelectedElement menuStateList of
                  Just (_, nextView) -> B.continue (switchView nextView st)
                  Nothing -> B.continue st

            B.VtyEvent vtyev -> do
                brickList_ <- B.handleListEvent vtyev menuStateList
                B.continue $ st{ view = menu{ menuStateList = brickList_ } }
            _ -> B.continue st
        _ -> case ev of
            B.VtyEvent (B.EvKey B.KEsc _) -> B.halt st
            _ -> B.continue st

bwapp = B.App
    { appDraw = renderState
    , appChooseCursor = B.neverShowCursor
    , appStartEvent = return
    , appAttrMap
      = const $ B.attrMap B.defAttr [("inverted", B.defAttr `B.withStyle` B.reverseVideo)]
    , appHandleEvent = handleEvents
    }
