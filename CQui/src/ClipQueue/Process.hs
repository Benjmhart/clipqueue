{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ClipQueue.Process where

import           ClassyPrelude
import qualified Data.Char                     as CH
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Brick.BChan                    (BChan, writeBChan)
import           Network.HTTP.Listen            (Listener, run)
import           System.Directory               (getHomeDirectory)
import           System.Exit                    (die)
import           System.IO.Silently             (silence)
import           System.Process                 (callCommand, getPid, interruptProcessGroupOf, spawnCommand, ProcessHandle)
import           Control.Concurrent             ( forkIO, killThread, ThreadId )
import           ClipQueue.Types                (CQMode(..), CustomEvent)




showHelpText :: CQMode -> String -> IO ()
showHelpText mode versionText = when (mode == Help) $ do
  putStrLn $ tshow versionText
  putStrLn helpText
  die ""

helpText ::Text
helpText = T.unlines
  [ "ClipQueue Help"
  , "clipqueue [mode [filename] ]"
  , "Modes:"
  , "n | normal  - Default behaviour if no mode is specified. New clipboard actions triggered by keyboard shortcuts will automatically be recorded and added to the queue. New clipboard items will be set as the current clipboard item."
  , "b | build   - New cuts will not affect the current clipboard item, but will be stored for later use"
  , "s | static  - Static mode does not monitor new clipboard activity, however you can set the current clipboard item"
  , "q | queue   - Queue mode treats your stored clipboard as a queue, you will only paste from the top, and only cut or copy items to the bottom. When you paste, the topmost item will be removed and your clipboard selection will be moved to the next item"
  , "a | advance - Advance Mode does not remove selections, but is similar to Queue mode in that when you paste, your selection will advance automatically"
  , "the filename argument is used to choose an alternative filepath to use for the queue.  the default is ~/queue.txt, you can customize your own queue file,  use newlines to seperate the content,  for multiline clips, you can use the omega (Ω) character"
  , "Controls:"
  , "Up/Down Arrow keys - change selection, setting the current clipboard item"
  , "j & k keys will also behave as up & down, similar to Vim"
  , "t & b keys will select the first and last items in the queue"
  , "m - rotates through the various modes during runtime."
  , "q, ctrl-z, ctrl-c - quit"
  , "enter - resets the clipboard to the current item in case of some other clipboard interference"
  , "(outside of Clipqueue), use ctrl+c, ctrl+x, and ctrl+v as you would normally,  Clipqueue will monitor these keys and pull from your clipboard automatically, saving to the queue file as new items are recovered"
  , "ClipQueue only supports text clipboard items"
  ]

runSilent = silence . spawnCommand
  
cleanupThreadsAndProcesses :: MonadIO m => [ProcessHandle] -> [ThreadId] -> m ()
cleanupThreadsAndProcesses procs tids= do
  liftIO $ traverse_ interruptProcessGroupOf procs
  liftIO $ traverse_ killThread tids


launchListener :: Int -> CustomEvent -> BChan CustomEvent -> IO ThreadId
launchListener port ev
  = forkIO . run port . listen . emit ev

getModeArgs :: MonadIO m => [Text] -> m CQMode
getModeArgs = pure . maybe Normal parseMode . listToMaybe

getPathArgs :: MonadIO m => [Text] ->  m FilePath
getPathArgs =  maybe (map (</> "queue.txt") (liftIO getHomeDirectory)) (pure . T.unpack) . afterHead

emit :: CustomEvent -> BChan CustomEvent -> IO ()
emit = flip writeBChan

listen :: IO () -> Listener B.ByteString IO
listen effect _ = do
  effect
  return Nothing

parseMode :: Text -> CQMode
parseMode s = 
  let a = T.map CH.toLower . T.take 1 . T.filter (/= '-') $ s in
  case a of
                "n"       -> Normal
                "s"       -> Static
                "b"       -> Build
                "q"       -> Queue
                "a"       -> Advance
                "h"       -> Help
                _         -> Normal


afterHead :: [a] -> Maybe a
afterhead []      = Nothing
afterhead [_]  = Nothing
afterHead (_:a:_) = Just a
afterHead _       = Nothing
