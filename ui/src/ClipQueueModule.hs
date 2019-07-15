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
            , savePath :: Maybe FilePath}
  deriving (Show, Eq)

data CustomEvent = CutEvent | PasteEvent
  deriving(Show, Eq)

emit :: CustomEvent -> BChan CustomEvent -> IO ()
emit e chan = writeBChan chan $ e

listen :: IO () -> Listener B.ByteString IO
listen effect _ = do
  effect
  return Nothing

buildInitialState :: Maybe CQMode -> Maybe FilePath -> IO TuiState
buildInitialState mode path = do
  queue <- readFileUtf8 $ "../queue.txt"
  evaluate (force queue)
  let 
    setMode = case mode of
      Just Static  -> Static
      Just Queue   -> Queue
      Just Advance -> Advance
      _            -> Normal
  case NE.nonEmpty . lines $ queue of
    Nothing -> die "there are no contents"
    Just ne -> pure TuiState 
                { tuiStateQueue = makeNonEmptyCursor ne 
                , mode = setMode
                , savePath = path
                }

parseMode :: Text -> Maybe CQMode
parseMode s = case ((T.filter (== '-')) . (T.map CH.toLower) $ s) of
                "normal"  -> Just Normal
                "static"  -> Just Static
                "queue"   -> Just Queue
                "advance" -> Just Advance
                "n"       -> Just Normal
                "s"       -> Just Static
                "q"       -> Just Queue
                "a"       -> Just Advance
                _         -> Nothing

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