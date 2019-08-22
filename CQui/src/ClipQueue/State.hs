{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ClipQueue.State where

import           ClassyPrelude
import qualified Data.Text as T
import           Data.List.NonEmpty             (NonEmpty(..)) -- includes (:|)
import qualified Data.List.NonEmpty            as NE
import           System.Hclip                   (setClipboard, getClipboard)

import           Brick.Main                     (continue)
import           Brick.Types                    (BrickEvent, EventM, Next)
import           Cursor.Simple.List.NonEmpty    (NonEmptyCursor(..))
import qualified Cursor.Simple.List.NonEmpty   as NEC
import           ClipQueue.Types                (CQMode(..), CustomEvent(..), TuiState(..), nextRuntimeCQMode)


-- | general Util

safeDecode :: Text -> String
safeDecode = T.unpack . T.map toNewLine
 where
  toNewLine 'Ω' = '\n'
  toNewLine a   = a

safeEncode :: Text -> Text
safeEncode = T.map fromNewLine
  where
  fromNewLine '\n' = 'Ω'
  fromNewLine a    = a

-- | Cursor and clipboard utils

cursorToList :: NonEmptyCursor a -> [a]
cursorToList = foldr (:) []  . NEC.rebuildNonEmptyCursor

setTuiState :: Monad m => TuiState -> NonEmptyCursor Text -> m TuiState
setTuiState s n = return $ s { tuiStateQueue = n }

setClipboardFromNec :: MonadIO m => NonEmptyCursor Text -> m ()
setClipboardFromNec = liftIO . setClipboard . safeDecode . NEC.nonEmptyCursorCurrent

mkCursorStateFromText :: Monad m => Text -> m (NonEmptyCursor Text)
mkCursorStateFromText = mkCursorState . lines

mkCursorState :: Monad m => [Text] -> m (NonEmptyCursor Text)
mkCursorState = map NEC.makeNonEmptyCursor . maybe (pure $ "" :| [] )  pure . NE.nonEmpty

setClipBoardAfter :: MonadIO m => (TuiState -> m (NonEmptyCursor Text)) ->  TuiState -> m TuiState
setClipBoardAfter f s = do
  newNec <- f s
  setClipboardFromNec newNec
  setTuiState s newNec

forceWrite :: MonadIO m => FilePath -> NonEmptyCursor Text -> m (NonEmptyCursor Text)
forceWrite path newQueue = do
  written <- writeCursorToFile path newQueue
  evaluate (force written)
  return newQueue

writeCursorToFile :: MonadIO m => FilePath -> NonEmptyCursor Text -> m()
writeCursorToFile path cursor = liftIO $ writeFileUtf8 path textToWrite
  where textToWrite = T.unlines $ cursorToList cursor


-- | event Util 

handleEventWith :: (TuiState -> EventM n TuiState) -> TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
handleEventWith f s e = do
  newState <- f s 
  continue newState

-- | Movement Handlers

advanceState :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
advanceState = pure . advanceQueue . tuiStateQueue

advanceQueue :: NonEmptyCursor Text -> NonEmptyCursor Text    
advanceQueue nec = fromMaybe nec (NEC.nonEmptyCursorSelectNext nec)

recedeState :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
recedeState = pure . recedeQueue . tuiStateQueue

recedeQueue :: NonEmptyCursor Text -> NonEmptyCursor Text
recedeQueue nec = fromMaybe nec (NEC.nonEmptyCursorSelectPrev nec)

topState :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
topState = pure . NEC.nonEmptyCursorSelectFirst . tuiStateQueue

top :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
top = handleEventWith $ setClipBoardAfter topState

bottomState :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
bottomState = pure . NEC.nonEmptyCursorSelectLast . tuiStateQueue

bottom :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
bottom = handleEventWith $ setClipBoardAfter bottomState

resetState :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
resetState = pure . tuiStateQueue

resetClipboard :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
resetClipboard = handleEventWith $ setClipBoardAfter resetState

updateFromFile :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
updateFromFile  = handleEventWith updateQueue

up :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
up = handleEventWith $ setClipBoardAfter recedeState

down :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
down = handleEventWith $ setClipBoardAfter advanceState

-- Cut Handlers

processCut :: MonadIO m => TuiState -> m TuiState
processCut s = case mode s of
  Normal -> setClipBoardAfter (map NEC.nonEmptyCursorSelectLast . getNewCut) s 
  Static -> setClipBoardAfter resetState s
  _ -> setClipBoardAfter getNewCut s 

getQueueFromFile :: MonadIO m => FilePath -> m (NonEmptyCursor Text)
getQueueFromFile path = do 
  tx <- liftIO . readFileUtf8 $ path
  liftIO $ evaluate $ force tx
  mkCursorStateFromText tx

updateQueue :: MonadIO m => TuiState -> m TuiState
updateQueue s = do
  cursorState <- getQueueFromFile $ savePath s
  setClipboardFromNec cursorState
  setTuiState s cursorState

getNewCut :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
getNewCut s = do
  newItem' <- liftIO $ T.pack <$> getClipboard
  let 
    newItem = safeEncode newItem'
  if null newItem then return $ tuiStateQueue s else do
    let 
      newQueue = NEC.nonEmptyCursorAppendAtEnd newItem' (tuiStateQueue s)
    forceWrite (savePath s) newQueue

-- Paste Handlers

processPaste :: MonadIO m => TuiState -> m TuiState
processPaste s = case mode s of
  Queue -> processQueuePaste s -- remove first, write file, set clipboard
  Advance -> processAdvancePaste s -- advance, set clipboard
  _ -> pure s

processQueuePaste :: MonadIO m => TuiState -> m TuiState
processQueuePaste = setClipBoardAfter queuePaste

queuePaste :: MonadIO m => TuiState -> m (NonEmptyCursor Text)
queuePaste s = do
  newQueue <- mkCursorState . drop 1 . cursorToList $ tuiStateQueue s
  forceWrite (savePath s) newQueue

processAdvancePaste :: MonadIO m => TuiState -> m TuiState
processAdvancePaste = setClipBoardAfter advanceState

-- Others

buildInitialState :: MonadIO m => CQMode -> FilePath -> String -> m TuiState
buildInitialState mode path version = do
  queue <- getQueueFromFile path
  setClipboardFromNec queue
  return $ TuiState queue mode path version

rotateMode :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)
rotateMode s e = continue $ s { mode = nextRuntimeCQMode $ mode s }























 