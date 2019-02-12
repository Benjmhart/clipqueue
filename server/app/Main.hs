{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Hclip
import Web.Scotty
-- import Data.Monoid (mconcat)
import Control.Monad(liftM)
import Control.Monad.IO.Class
import Data.Maybe(listToMaybe)
import Safe (tailSafe)
import System.IO(writeFile)
import Control.DeepSeq
import Control.Exception

data CBEvent = PasteEvent | CopyEvent | CutEvent deriving(Eq, Read, Show)

main :: IO ()
main = scotty 44499 $ do
  get "/:event" $ do
    event <- param "event"
    queue <- liftIO $ readFile $ "../queue.txt"
    liftIO $ evaluate (force queue)
    case (read event :: CBEvent) of
      PasteEvent -> liftIO $ handlePaste $ lines queue
      CopyEvent  -> liftIO $ handleCut $ lines queue
      CutEvent   -> liftIO $ handleCut $ lines queue
    html $ mconcat ["<h1>", "response", "</h1>"]


handlePaste :: [String] -> IO ()
handlePaste queue = do
  let newQueue = tailSafe queue
  putStrLn . unlines $ newQueue
  written <- writeFile "../queue.txt" (unlines newQueue)
  evaluate (force written)
  case listToMaybe newQueue of
    Nothing -> return ()
    Just a  -> setClipboard a

handleCut :: [String] -> IO ()
handleCut  queue = do
  newItem <- getClipboard
  putStrLn $ "new item from clipBoard: " ++ newItem
  let newQueue = queue ++ [newItem]
  setClipboard $ head newQueue
  putStrLn $ "reset head of CB to: " ++ (head newQueue)
  written <- writeFile "../queue.txt" (unlines newQueue)
  evaluate (force written)
  return ()



  