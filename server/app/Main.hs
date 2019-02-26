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
    let revisedQ = filter (not . null) . lines $ queue
    liftIO $ evaluate (force queue)
    case (read event :: CBEvent) of
      PasteEvent -> liftIO $ handlePaste $ revisedQ
      CopyEvent  -> liftIO $ handleCut $ revisedQ
      CutEvent   -> liftIO $ handleCut $ revisedQ
    html $ mconcat ["<h1>", "response", "</h1>"]


handlePaste :: [String] -> IO ()
handlePaste queue = do
  let newQueue = tailSafe queue
  putStrLn . unlines $ newQueue
  written <- writeFile "../queue.txt" (unlines newQueue)
  evaluate (force written)
  case listToMaybe newQueue of
    Nothing -> return ()
    Just a  -> setClipboard . safeDecode $ a

safeDecode :: String -> String
safeDecode []       = []
safeDecode ('Ω':xs) = ('\n':safeDecode xs)
safeDecode (x:xs)   = ( x  :safeDecode xs)

safeEncode :: String -> String
safeEncode []        = []
safeEncode ('\n':xs) = ('Ω':safeEncode xs)
safeEncode (x:xs)    = ( x :safeEncode xs)

     
handleCut :: [String] -> IO ()
handleCut  queue = do
  newItem' <- getClipboard
  let newItem = safeEncode newItem'
  case null newItem of
    True -> return ()
    False -> do
      putStrLn $ "new item from clipBoard: " ++ newItem
      let newQueue = queue ++ [newItem]
      setClipboard . safeDecode . head  $ newQueue
      putStrLn $ "reset head of CB to: " ++ (head newQueue)
      written <- writeFile "../queue.txt" (unlines newQueue)
      evaluate (force written)
      return ()



  