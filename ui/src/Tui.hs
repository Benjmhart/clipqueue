{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Exit

import System.Hclip

import Data.List(sort)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Cursor.Simple.List.NonEmpty
import Control.Monad(liftM)
import Control.Monad.IO.Class

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Border
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Control.DeepSeq
import Control.Exception

tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState
    

data TuiState =
    TuiState { tuiStateQueue :: NonEmptyCursor FilePath}
    deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty [("selected", fg red)]
        }

buildInitialState :: IO TuiState
buildInitialState = do
  queue <- readFile $ "../queue.txt"
  evaluate (force queue)
  case NE.nonEmpty . lines $ queue of
    Nothing -> die "there are no contents"
    Just ne -> pure TuiState { tuiStateQueue = makeNonEmptyCursor ne}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  let nec = tuiStateQueue ts
  in [border $ vBox $ 
     concat
          [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
          , [ drawPath True $ nonEmptyCursorCurrent nec ]
          , map (drawPath False) $ nonEmptyCursorNext nec ]]

drawPath :: Bool -> FilePath -> Widget n
drawPath b 
  | b == True = withAttr "selected" . str
  | otherwise = str

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey KDown [] -> do
                  let nec = tuiStateQueue s
                  case nonEmptyCursorSelectNext nec of
                    Nothing -> continue s
                    Just nec' -> do
                      liftIO . setClipboard . nonEmptyCursorCurrent $ nec'
                      continue $ s { tuiStateQueue = nec' }
                EvKey KUp [] -> do
                  let nec = tuiStateQueue s
                  case nonEmptyCursorSelectPrev nec of
                    Nothing -> continue s
                    Just nec' -> do
                      liftIO . setClipboard . nonEmptyCursorCurrent $ nec'
                      continue $ s { tuiStateQueue = nec' }
                -- EvKey KEnter [] -> do
                --   let selection = nonEmptyCursorCurrent $ tuiStateQueue s
                --   continue s
                --   case isDir of
                --     False -> continue s
                --     True -> do
                --       liftIO $ setCurrentDirectory selection
                --       s' <- liftIO buildInitialState
                --       continue s'
                _ -> continue s
        _ -> continue s
