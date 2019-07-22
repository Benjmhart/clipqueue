{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           ClassyPrelude
import qualified Data.Text                     as T
import           System.Hclip                   (getClipboard)
import           Web.Scotty
import           System.Directory               (getHomeDirectory)
import           System.FilePath                ((</>))
import qualified Data.List                     as L
import qualified Data.Char                     as CH
import           Control.Monad                  ( liftM )
import           Control.Monad.IO.Class
import           Data.Maybe                     ( listToMaybe )
import           Safe                           ( tailSafe )
import           Control.DeepSeq

data CBEvent = PasteEvent | CopyEvent | CutEvent deriving(Eq, Read, Show)

main :: IO ()
main = do
  rawArgs <- getArgs
  homeDir <- getHomeDirectory
  savePath <- maybe (pure $ homeDir </>"queue.txt") (pure . T.unpack) (listToMaybe rawArgs)
  putStrLn . T.pack $ "server running with path: " <> savePath
  runScotty savePath

runScotty :: FilePath -> IO ()
runScotty path = scotty 44499 $ get "/:event" $ do
    event <- param "event"
    queue <- liftIO $ readFileUtf8 $ path
    let revisedQ = filter (not . null) . lines $ queue
    liftIO $ evaluate (force queue)
    case (readMay (event :: String) :: Maybe CBEvent) of
      Just PasteEvent -> handlePaste revisedQ path
      Just CopyEvent  -> handleCut revisedQ path
      Just CutEvent   -> handleCut revisedQ path
      _               -> return ()

handlePaste :: MonadIO m => [Text] -> FilePath -> m ()
handlePaste queue path = liftIO $ do
  let newQueue = tailSafe queue
  putStrLn . unlines $ newQueue
  written <- writeFileUtf8 path (unlines newQueue)
  evaluate (force written)

handleCut :: MonadIO m => [Text] -> FilePath -> m ()
handleCut queue path = liftIO $ do
  newItem' <- getClipboard
  let newItem = safeEncode . T.pack $ newItem'
  liftIO $ putStrLn $ "new Item! " <> newItem
  unless (null newItem) $ do
    putStrLn $ ("new item from clipBoard: " <> newItem :: Text)
    let newQueue = queue ++ [newItem]
    written <- writeFileUtf8 path (T.unlines newQueue)
    evaluate (force written)

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
