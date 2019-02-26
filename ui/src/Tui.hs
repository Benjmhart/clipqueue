{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Exit

import System.Hclip

import Data.List(sort)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(fromJust)
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
    TuiState { tuiStateQueue :: NonEmptyCursor String}
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
          [ map (drawItem False) $ reverse $ nonEmptyCursorPrev nec
          , [ drawItem True $ nonEmptyCursorCurrent nec ]
          , map (drawItem False) $ nonEmptyCursorNext nec ]]

drawItem :: Bool -> String -> Widget n
drawItem isSelected 
  | isSelected == True = withAttr "selected" . str . prePrep
  | otherwise          = str . prePrep
  where prePrep        = addEllipses . take 18 . filter isntWhite
        addEllipses xs
          | length xs >= 18 = xs ++ "..."
          | otherwise     = xs

isntWhite :: Char -> Bool
isntWhite ' ' = False
isntWhite 'Ω' = False
isntWhite x   = True

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e = 
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey (KChar 'u') [] -> do
                  newQ <- liftIO $ readFile $ "../queue.txt"
                  liftIO $ evaluate (force newQ)
                  case NE.nonEmpty . lines $ newQ of
                    Nothing -> continue $ s { tuiStateQueue = makeNonEmptyCursor ( fromJust . NE.nonEmpty $ [""])}
                    Just ne -> continue $ s { tuiStateQueue = makeNonEmptyCursor ne}
                EvKey KDown [] -> do
                  let nec = tuiStateQueue s
                  case nonEmptyCursorSelectNext nec of
                    Nothing -> do
                      liftIO . setClipboard . safeDecode . nonEmptyCursorCurrent $ nec
                      continue s
                    Just nec' -> do
                      liftIO . setClipboard . safeDecode . nonEmptyCursorCurrent $ nec'
                      continue $ s { tuiStateQueue = nec' }
                EvKey KUp [] -> do
                  let nec = tuiStateQueue s
                  case nonEmptyCursorSelectPrev nec of
                    Nothing ->  do
                      liftIO . setClipboard . safeDecode . nonEmptyCursorCurrent $ nec
                      continue s
                    Just nec' -> do
                      liftIO . setClipboard . safeDecode . nonEmptyCursorCurrent $ nec'
                      continue $ s { tuiStateQueue = nec' }
                EvKey KEnter [] -> do
                    let nec =  tuiStateQueue s
                    liftIO . setClipboard . safeDecode . nonEmptyCursorCurrent $ nec
                    continue s
                _ -> continue s
        _ -> continue s

safeDecode :: String -> String
safeDecode []       = []
safeDecode ('Ω':xs) = ('\n':safeDecode xs)
safeDecode (x:xs)   = ( x  :safeDecode xs)