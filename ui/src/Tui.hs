{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tui where

import           ClassyPrelude
import qualified Data.Text                     as T
import           System.Exit
import           GHC.Read
import           System.Hclip
import qualified Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.List                      ( sort )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Maybe                     ( fromJust )
import qualified Data.Char                     as CH
import qualified Data.List.NonEmpty            as NE
import           Cursor.Simple.List.NonEmpty
import           Control.Monad                  ( liftM )
import           Control.Monad.IO.Class
import           Control.Concurrent             ( forkIO )
import           Control.DeepSeq
import           Brick.AttrMap
import           Brick.BChan
import           Brick.Main
import           Brick.Types
import           Brick.Util
import           Brick.Widgets.Core
import           Brick.Widgets.Border
import           Graphics.Vty
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Network.HTTP.Listen
import           ClipQueueModule
import           UnliftIO.Process (spawnCommand)
import           System.IO.Silently (silence)
import qualified System.Process.Typed as PT
import           System.Directory               (getHomeDirectory)
import           System.FilePath                ((</>))

tui :: IO ()
tui = do
  args <- getArgs
  mode <- maybe (pure Normal) (pure . parseMode) (listToMaybe args) 
  savePath <- maybe (map (</> "queue.txt") getHomeDirectory) (pure . T.unpack) (afterHead args)
  showHelpText mode
  -- TODO: use unless
  unless (mode == Static) $ do
        void . forkIO $ do
          _ <- silence . void $ PT.withProcessTerm (PT.shell $ "( cd ../keyListener ; npm start )") PT.stopProcess
          _ <- silence . void $ PT.withProcessTerm (PT.shell $ "( cd ../keyListener ; npm start )") PT.stopProcess
          return ()
  initialState <- buildInitialState mode savePath
  eventChan    <- newBChan 10
  let 
    buildVty = mkVty defaultConfig
  initialVty <- buildVty
  forkIO $ run 55999 $ listen $ emit CutEvent eventChan
  forkIO $ run 55998 $ listen $ emit PasteEvent eventChan
  endState <- customMain initialVty
                         buildVty
                         (Just eventChan)
                         tuiApp
                         initialState
  print endState
  return ()

type ResourceName = Text

tuiApp :: App TuiState CustomEvent ResourceName
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = showFirstCursor
             , appHandleEvent  = handleTuiEvent
             , appStartEvent   = pure
             , appAttrMap      = const $ attrMap mempty [("selected", fg red)]
             }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  let nec = tuiStateQueue ts
  in  [ border $ vBox $ concat
          [ map (drawItem False . T.unpack) $ reverse $ nonEmptyCursorPrev nec
          , [drawItem True . T.unpack $ nonEmptyCursorCurrent nec]
          , map (drawItem False . T.unpack) $ nonEmptyCursorNext nec
          ]
      ]

drawItem :: Bool -> String -> Widget n
drawItem isSelected | isSelected == True = withAttr "selected" . str . prePrep
                    | otherwise          = str . prePrep
 where
 --TODO: remove magic number
  prePrep = addEllipses . take 18 . filter isntWhite
  addEllipses xs | length xs >= 18 = xs ++ "..."
                 | otherwise       = xs


-- TODO: break this function up into smaller functions for each case and remove duplication
handleTuiEvent
  :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
handleTuiEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> do
      --TODO compile binaries and run directly
      _ <- spawnCommand "( killall node )" 
      _ <- spawnCommand "( killall stack )"
      halt s
    EvKey (KChar 'u') [] -> do
      newQ <- liftIO $ readFileUtf8 $ "../queue.txt"
      liftIO $ evaluate (force newQ)
      case NE.nonEmpty . lines $ newQ of
        Nothing -> continue $ s
        -- use  (:|) to safely operate on the nonempty
          { tuiStateQueue = makeNonEmptyCursor $ fromJust . NE.nonEmpty $ [""]
          }
        Just ne -> continue $ s { tuiStateQueue = makeNonEmptyCursor ne }
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
        Nothing -> do
          liftIO . setClipboard . safeDecode . nonEmptyCursorCurrent $ nec
          continue s
        Just nec' -> do
          liftIO . setClipboard . safeDecode . nonEmptyCursorCurrent $ nec'
          continue $ s { tuiStateQueue = nec' }
    EvKey KEnter [] -> do
      let nec = tuiStateQueue s
      liftIO . setClipboard . safeDecode . nonEmptyCursorCurrent $ nec
      continue s
    _ -> continue s
  (AppEvent (CutEvent)) -> do
    let nec       = tuiStateQueue s
    let newCursor = makeNonEmptyCursor $ fromJust . NE.nonEmpty $ ["CUT!"]
    let nextState = nonEmptyCursorAppendAtEnd "CUT!" nec
    continue s { tuiStateQueue = nextState }
  (AppEvent (PasteEvent)) -> do
    let nec       = tuiStateQueue s
    let newCursor = makeNonEmptyCursor $ fromJust . NE.nonEmpty $ ["PASTE!"]
    let nextState = nonEmptyCursorAppendAtEnd "PASTE!" nec
    continue s { tuiStateQueue = nextState }
  _ -> continue s

