{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ClipQueue.Types where

import           ClassyPrelude
import           Cursor.Simple.List.NonEmpty (NonEmptyCursor)

data CQMode = Normal | Static | Queue | Advance | Build | Help 
  deriving (Show, Read, Eq, Enum)

nextRuntimeCQMode :: CQMode -> CQMode
nextRuntimeCQMode Build = Normal
nextRuntimeCQMode a     = succ a

data TuiState =
  TuiState  { tuiStateQueue :: NonEmptyCursor Text
            , mode :: CQMode
            , savePath :: FilePath
            , versionText :: String
            }
  deriving (Show, Eq)

data CustomEvent = CutEvent | PasteEvent
  deriving(Show, Eq)