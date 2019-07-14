{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude
import qualified Data.Text as T
import System.Hclip
import Web.Scotty
-- import Data.Monoid (mconcat)
import Control.Monad(liftM)
import Control.Monad.IO.Class
import Data.Maybe(listToMaybe)
import Safe (tailSafe)
import Control.DeepSeq

data CBEvent = PasteEvent | CopyEvent | CutEvent deriving(Eq, Read, Show)

main :: IO ()
main = scotty 44499 $ do
  get "/:event" $ do
    event <- param "event"
    queue <- liftIO $ readFileUtf8 $ "../queue.txt"
    let revisedQ = filter (not . null) . lines $ queue
    liftIO $ evaluate (force queue)
    case (readMay (event :: String) :: Maybe CBEvent) of
      Just PasteEvent -> liftIO $ handlePaste $ revisedQ
      Just CopyEvent  -> liftIO $ handleCut $ revisedQ
      Just CutEvent   -> liftIO $ handleCut $ revisedQ
      _ -> return ()
    html $ mconcat ["<h1>", "response", "</h1>"]


handlePaste :: [Text] -> IO ()
handlePaste queue = do
  let 
    newQueue = tailSafe queue
  putStrLn . unlines $ newQueue
  written <- writeFileUtf8 "../queue.txt" (unlines newQueue)
  evaluate (force written)
  case listToMaybe newQueue of
    Nothing -> return ()
    Just a  -> setClipboard . safeDecode $ a

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

     
handleCut :: [Text] -> IO ()
handleCut  queue = do
  newItem' <- getClipboard
  let 
    newItem = safeEncode . T.pack $ newItem'
  case null newItem of
    True -> return ()
    False -> do
      putStrLn $ ("new item from clipBoard: " <> newItem :: Text)
      let 
        newQueue = queue ++ [newItem]
        first = safeDecode <$> listToMaybe newQueue
      case first of
        Nothing -> return ()
        Just s  -> do
          written <- writeFileUtf8 "../queue.txt" (T.unlines newQueue)
          evaluate (force written)
          return ()



  