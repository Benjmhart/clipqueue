{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tui where

import           ClassyPrelude
import           Brick.AttrMap                  (attrMap)
import           Brick.BChan                    (BChan, newBChan)
import           Brick.Main                     (App(..), customMain, showFirstCursor, halt, continue)
import           Brick.Types                    (Widget(..), BrickEvent(..), EventM(..), Next(..))
import           Brick.Util                     (fg)
import           Graphics.Vty                   (Event(..), Key(..), Modifier(..), mkVty, defaultConfig, red)
import           System.Environment.Executable  (getExecutablePath)
import           System.FilePath                (dropFileName, (</>))
import           ClipQueue.Types                (CQMode(..), CustomEvent(..), TuiState(..))
import qualified ClipQueue.State               as CQS
import qualified ClipQueue.Process             as CQP
import qualified ClipQueue.Render              as CQR

keyListenerPath :: FilePath -> FilePath
keyListenerPath execpath
  | (isInfixOf) ".stack-work" execpath = "node ../CQKeyListener/ index.js" 
  | otherwise                          = "node /home/ben/Projects/clipqueue/CQKeyListener/index.js"--(dropFileName execpath) </> "cqlistener"

version :: String
version = "Clipqueue 0.5.0"

tui :: IO ()
tui = do
  execpath <- getExecutablePath
  putStrLn . tshow $ "\n\n EXECUTABLE PATH: " ++ execpath
  args <- getArgs
  mode <- CQP.getModeArgs args
  savePath <- CQP.getPathArgs args
  CQP.showHelpText mode version
  initialState <- CQS.buildInitialState mode savePath version
  eventChan    <- newBChan 10
  keyListenerProc <- CQP.runSilent $ keyListenerPath execpath
  cutThread <- CQP.launchListener 55999 CutEvent eventChan
  pasteThread <- CQP.launchListener 55998 PasteEvent eventChan
  endState <- launchCustomBrick initialState eventChan
  CQP.cleanupThreadsAndProcesses [keyListenerProc] [cutThread, pasteThread]

  return ()

launchCustomBrick :: MonadIO m => TuiState -> BChan CustomEvent -> m TuiState
launchCustomBrick initialState eventChan = do
  let buildVty = mkVty defaultConfig
  initialVty <- liftIO buildVty
  liftIO $ customMain initialVty
          buildVty
          (Just eventChan)
          tuiApp
          initialState

tuiApp :: App TuiState CustomEvent Text
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = showFirstCursor
             , appHandleEvent  = handleTuiEvent
             , appStartEvent   = pure
             , appAttrMap      = const $ attrMap mempty [("selected", fg red)]
             }

drawTui :: TuiState -> [Widget Text]
drawTui ts =
  let nec = tuiStateQueue ts
  in  [ CQR.progInfo ts
      , CQR.progStatus nec
      ]

handleTuiEvent
  :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
handleTuiEvent s e = case e of
    VtyEvent vtye -> case vtye of
      EvKey (KChar 'q') [] -> halt s
      EvKey (KChar 'z') [MCtrl] -> halt s
      EvKey (KChar 'c') [MCtrl] -> halt s
      EvKey (KChar 't') [] -> CQS.top s e
      EvKey (KChar 'b') [] -> CQS.bottom s e
      EvKey (KChar 'm') [] -> CQS.rotateMode s e
      EvKey (KChar 'u') [] -> CQS.updateFromFile s e
      EvKey (KChar 'j') [] -> CQS.up s e
      EvKey (KChar 'k') [] -> CQS.down s e
      EvKey KDown [] -> CQS.down s e
      EvKey KUp [] -> CQS.up s e
      EvKey KEnter [] -> CQS.resetClipboard s e
      _ -> continue s
    (AppEvent CutEvent) -> CQS.handleEventWith CQS.processCut s e 
    (AppEvent PasteEvent) -> CQS.handleEventWith CQS.processPaste s e
    _ -> continue s

