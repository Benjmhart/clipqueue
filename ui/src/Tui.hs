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
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Char                     as CH
import qualified Data.List.NonEmpty            as NE
import           Cursor.Simple.List.NonEmpty
import           Control.Monad                  ( liftM )
import           Control.Monad.IO.Class
import           Control.Concurrent             ( forkIO, killThread, ThreadId )
import           Control.DeepSeq
import           Brick.AttrMap
import           Brick.BChan
import           Brick.Main
import           Brick.Types
import           Brick.Util
import           Brick.Widgets.Core
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Graphics.Vty
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           ClipQueueModule

-- TODO: use getExecutablePath from System.environment to run the keylistener or https://hackage.haskell.org/package/executable-path

keyListenerPath :: FilePath
keyListenerPath = "../keyListener/index-linux"

versionText :: String
versionText = "Clipqueue 0.5.0"

tui :: IO ()
tui = do
  args <- getArgs
  mode <- getModeArgs args
  savePath <- getPathArgs args
  showHelpText mode versionText
  initialState <- buildInitialState mode savePath
  eventChan    <- newBChan 10
  keyListenerProc <- runSilent keyListenerPath
  cutThread <- launchListener 55999 CutEvent eventChan
  pasteThread <- launchListener 55998 PasteEvent eventChan
  endState <- launchCustomBrick initialState eventChan
  cleanupThreadsAndProcesses [keyListenerProc] [cutThread, pasteThread]
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
  in  [ progInfo ts
      , progStatus nec
      ]

progInfo ts = border $ vBox
  [ str versionText
  , str $ "mode: " ++ show (mode ts)
  , str $ "File: " ++ savePath ts
  ]

progStatus nec= hCenter $  border $ vBox $ concat
  [ map (drawItem None . T.unpack) $ reverse $ nonEmptyCursorPrev nec
  , [drawItem Highlight . T.unpack $ nonEmptyCursorCurrent nec]
  , map (drawItem None . T.unpack) $ nonEmptyCursorNext nec
  ]


-- TODO: break this function up into smaller functions for each case and remove duplication
handleTuiEvent
  :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
handleTuiEvent s e = case e of
    VtyEvent vtye -> case vtye of
      EvKey (KChar 'q') [] -> halt s
      EvKey (KChar 'z') [MCtrl] -> halt s
      EvKey (KChar 'c') [MCtrl] -> halt s
      EvKey (KChar 'm') [] -> rotateMode s e
      EvKey (KChar 'u') [] -> updateFromFile s e
      EvKey (KChar 'j') [] -> up s e
      EvKey (KChar 'k') [] -> down s e
      EvKey KDown [] -> down s e
      EvKey KUp [] -> up s e
      EvKey KEnter [] -> resetClipboard s e
      _ -> continue s
    (AppEvent CutEvent) -> handleEventWith processCut s e 
    (AppEvent PasteEvent) -> handleEventWith processPaste s e
    _ -> continue s


resetClipboard = handleEventWith $ setClipBoardAfter resetState

updateFromFile  = handleEventWith updateQueue

up = handleEventWith $ setClipBoardAfter recedeState

down = handleEventWith $ setClipBoardAfter advanceState

rotateMode :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
rotateMode s e = continue $ s { mode = (nextRuntimeCQMode $ mode s)}

processPaste :: MonadIO m => TuiState -> m TuiState
processPaste s = case mode s of
  Queue -> processQueuePaste s -- remove first, write file, set clipboard
  Advance -> processAdvancePaste s -- advance, set clipboard
  _ -> pure s

processCut :: MonadIO m => TuiState -> m TuiState
processCut s = case mode s of
  Normal -> setClipBoardAfter (map nonEmptyCursorSelectLast . getNewCut) s 
  Static -> pure s 
  _ -> setClipBoardAfter getNewCut s 

handleEventWith :: (TuiState -> EventM n TuiState) -> TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
handleEventWith f s e = do
  newState <- f s 
  continue newState


advanceState :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
advanceState = pure . advanceQueue . tuiStateQueue

advanceQueue :: NonEmptyCursor Text -> NonEmptyCursor Text    
advanceQueue nec = fromMaybe nec (nonEmptyCursorSelectNext nec)

recedeState :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
recedeState = pure . recedeQueue . tuiStateQueue

recedeQueue :: NonEmptyCursor Text -> NonEmptyCursor Text
recedeQueue nec = fromMaybe nec (nonEmptyCursorSelectPrev nec)

resetState :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
resetState = pure. id . tuiStateQueue

setClipBoardAfter :: MonadIO m => (TuiState -> m (NonEmptyCursor Text)) ->  TuiState -> m TuiState
setClipBoardAfter f s = do
  newNec <- f s
  setClipboardFromNec newNec
  setTuiState s newNec

-- TODO it feels like adjustclipqueueState and setClipboardAfter do similar things, we should see if we can streamline this

-- TODO - break up libraries into process management and state management if possible




processQueuePaste :: MonadIO m => TuiState -> m TuiState
processQueuePaste = setClipBoardAfter queuePaste

queuePaste :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
queuePaste s = do
  newQueue <- mkCursorState . drop 1 . cursorToList $ tuiStateQueue s
  written <- writeCursorToFile (savePath s) newQueue
  evaluate (force written)
  return newQueue

processAdvancePaste :: MonadIO m => TuiState -> m TuiState
processAdvancePaste = setClipBoardAfter advanceState




updateQueue :: MonadIO m => TuiState -> m TuiState
updateQueue s = do
  newQ <- getQueueFromFile s
  liftIO $ evaluate $ force newQ
  cursorState <- mkCursorStateFromText newQ
  setClipboardFromNec cursorState
  setTuiState s cursorState




--TODO Normal mode set cursor to new item

getNewCut :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
getNewCut s = do
  newItem' <- liftIO $ T.pack <$> getClipboard
  let 
    newItem = safeEncode newItem'
  if null newItem then return $ tuiStateQueue s else do
    let 
      newQueue = nonEmptyCursorAppendAtEnd newItem' (tuiStateQueue s)
    written <- writeCursorToFile (savePath s) newQueue
    evaluate (force written)
    return newQueue



writeCursorToFile path cursor = liftIO $ writeFileUtf8 path textToWrite
 where textToWrite = T.unlines $ cursorToList cursor


cursorToList :: NonEmptyCursor a -> [a]
cursorToList = foldr (:) []  . rebuildNonEmptyCursor

setTuiState :: Monad m => TuiState -> NonEmptyCursor Text -> m TuiState
setTuiState s n = return $ s { tuiStateQueue = n }


getQueueFromFile :: MonadIO m => TuiState -> m Text
getQueueFromFile s = do 
  tx <- liftIO . readFileUtf8 $ savePath s
  liftIO $ evaluate $ force tx
  return tx