{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ClipQueueModule where

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
import           Control.DeepSeq
import           Network.HTTP.Listen

data CQMode = Normal | Static | Queue | Advance | Help
  deriving (Show, Read, Eq)

data TuiState =
  TuiState { tuiStateQueue :: NonEmptyCursor Text
            , mode :: CQMode
            , savePath :: FilePath}
  deriving (Show, Eq)

data CustomEvent = CutEvent | PasteEvent
  deriving(Show, Eq)


showHelpText :: CQMode -> IO ()
showHelpText mode = when (mode == Help) $ do
  putStrLn helpText
  die ""

helpText ::Text
helpText = T.unlines
  [ "ClipQueue Help"
  , "clipqueue [mode [filename] ]"
  , "Modes:"
  , "n | normal  - Default behaviour if no mode is specified. New clipboard actions triggered by keyboard shortcuts will automatically be recorded and added to the queue. New clipboard items will be set as the current clipboard item."
  , "s | static  - Static mode does not monitor new clipboard activity, however you can set the current clipboard item"
  , "q | queue   - Queue mode treats your stored clipboard as a queue, you will only paste from the top, and only cut or copy items to the bottom. When you paste, the topmost item will be removed and your clipboard selection will be moved to the next item"
  , "a | advance - Advance Mode does not remove selections, but is similar to Queue mode in that when you paste, your selection will advance automatically"
  , "the filename argument is used to choose an alternative filepath to use for the queue.  the default is ~/queue.txt"
  ]

emit :: CustomEvent -> BChan CustomEvent -> IO ()
emit e chan = writeBChan chan $ e

listen :: IO () -> Listener B.ByteString IO
listen effect _ = do
  effect
  return Nothing

buildInitialState :: CQMode -> FilePath -> IO TuiState
buildInitialState mode path = do
  queue <- readFileUtf8 $ "../queue.txt"
  evaluate (force queue)
  let 
  case NE.nonEmpty . lines $ queue of
    Nothing -> die "there are no contents"
    Just ne -> pure TuiState 
                { tuiStateQueue = makeNonEmptyCursor ne 
                , mode = mode
                , savePath = path
                }

parseMode :: Text -> CQMode
parseMode s = case ((T.filter (== '-')) . (T.map CH.toLower) $ s) of
                "normal"  -> Normal
                "static"  -> Static
                "queue"   -> Queue
                "advance" -> Advance
                "n"       -> Normal
                "s"       -> Static
                "q"       -> Queue
                "a"       -> Advance
                _         -> Normal

isntWhite :: Char -> Bool
isntWhite ' ' = False
isntWhite 'Ω' = False
isntWhite x   = True

safeDecode :: Text -> String
safeDecode = T.unpack . T.map toNewLine
 where
  toNewLine 'Ω' = '\n'
  toNewLine a   = a

afterHead :: [a] -> Maybe a
afterhead []      = Nothing
afterhead (_:[])  = Nothing
afterHead (_:a:_) = Just a