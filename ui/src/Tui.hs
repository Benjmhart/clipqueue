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
import           Control.Concurrent             ( forkIO, killThread )
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
import           Network.HTTP.Listen
import           ClipQueueModule
import           UnliftIO.Process (spawnCommand, terminateProcess)
import           System.IO.Silently (silence)
import qualified System.Process.Typed as PT
import           System.Directory               (getHomeDirectory)
import           System.FilePath                ((</>))
import           System.Process  (callCommand, getPid, interruptProcessGroupOf)
import System.Posix.Signals (sigKILL, signalProcess)

-- TODO: use getExecutablePath from System.environment to run the keylistener

keyListenerPath = "../keyListener/index-linux"


tui :: IO ()
tui = do
  args <- getArgs
  mode <- maybe (pure Normal) (pure . parseMode) (listToMaybe args) 
  savePath <- maybe (map (</> "queue.txt") getHomeDirectory) (pure . T.unpack) (afterHead args)
  showHelpText mode
  keyListenerProc <- silence $ spawnCommand (keyListenerPath) 
  initialState <- buildInitialState mode savePath
  eventChan    <- newBChan 10
  cutThread <- forkIO $ run 55999 $ listen $ emit CutEvent eventChan
  pasteThread <- forkIO $ run 55998 $ listen $ emit PasteEvent eventChan
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
  interruptProcessGroupOf keyListenerProc
  killThread cutThread
  killThread pasteThread
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
  in  [ progInfo ts
      , progStatus nec
      ]

progInfo ts = border $ vBox
  [ str $ "Clipqueue 0.3.0"
  , str $  concat ["mode: ", show $ mode ts]
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
handleTuiEvent s e = do
  case e of
    VtyEvent vtye -> case vtye of
      EvKey (KChar 'q') [] -> halt s
      EvKey (KChar 'z') [] -> halt s
      EvKey (KChar 'u') [] -> handleEventWith updateQueue s e
      EvKey KDown [] -> handleEventWith (adjustClipQueueState advanceQueue) s e
      EvKey KUp [] -> handleEventWith (adjustClipQueueState recedeQueue) s e
      EvKey KEnter [] -> handleEventWith (adjustClipQueueState id) s e
      _ -> continue s
    (AppEvent (CutEvent)) -> handleEventWith getNewCut s e 
    (AppEvent (PasteEvent)) -> handleEventWith updateQueue s e
    _ -> continue s

handleEventWith :: (TuiState -> EventM n TuiState) -> TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
handleEventWith f s e = do
  newState <- f s 
  continue newState

advanceQueue :: NonEmptyCursor Text -> NonEmptyCursor Text    
advanceQueue nec = maybe nec id (nonEmptyCursorSelectNext nec)

recedeQueue :: NonEmptyCursor Text -> NonEmptyCursor Text
recedeQueue nec = maybe nec id (nonEmptyCursorSelectPrev nec)

adjustClipQueueState :: MonadIO m => (NonEmptyCursor Text -> NonEmptyCursor Text) -> TuiState -> m TuiState
adjustClipQueueState f s = do
  let 
    nec = tuiStateQueue s
    newnec = f nec
  setClipboardFromNec newnec
  setTuiState s newnec

setClipboardFromNec :: MonadIO m => NonEmptyCursor Text -> m ()
setClipboardFromNec = liftIO . setClipboard . safeDecode . nonEmptyCursorCurrent

updateQueue :: MonadIO m => TuiState -> m TuiState
updateQueue s = do
  newQ <- getQueueFromFile s
  liftIO $ evaluate $ force newQ
  cursorState <- mkCursorState newQ
  setClipboardFromNec cursorState
  setTuiState s cursorState

getNewCut :: MonadIO m => TuiState -> m TuiState
getNewCut s = do
  newItem' <- liftIO $ T.pack <$> getClipboard
  let 
    newItem = safeEncode $ newItem'
    path = savePath s
  if (null newItem) then return s else do
    let 
      newQueue = nonEmptyCursorAppendAtEnd newItem' (tuiStateQueue s)
      textToWrite = T.unlines $ foldr (:) [] (rebuildNonEmptyCursor newQueue)
    written <- liftIO $ writeFileUtf8 path textToWrite
    evaluate (force written)
    cursorState <- mkCursorState $ textToWrite
    setClipboardFromNec cursorState
    setTuiState s cursorState

setTuiState :: Monad m => TuiState -> NonEmptyCursor Text -> m TuiState
setTuiState s n = return $ s { tuiStateQueue = n }


getQueueFromFile :: MonadIO m => TuiState -> m Text
getQueueFromFile s = do 
  tx <- liftIO . readFileUtf8 $ savePath s
  liftIO $ evaluate $ force tx
  return tx