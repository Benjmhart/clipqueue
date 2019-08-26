{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ClipQueue.Render where

import           ClassyPrelude
import qualified Data.Text                     as T
import           Brick.Types                    (Widget)
import           Brick.Widgets.Border           (border)
import           Brick.Widgets.Core             (withAttr, str, vBox)
import           Brick.Widgets.Center           (hCenter)
import           Cursor.Simple.List.NonEmpty    (NonEmptyCursor(..))
import qualified Cursor.Simple.List.NonEmpty   as NEC
import           ClipQueue.Types                (TuiState(..))


drawTui :: TuiState -> [Widget Text]
drawTui ts =
  let nec = tuiStateQueue ts
  in  [ progInfo ts
      , progStatus nec
      ]


progInfo :: TuiState -> Widget n
progInfo ts = border $ vBox
  [ str $ versionText ts
  , str $ "mode: " ++ show (mode ts)
  , str $ "File: " ++ savePath ts
  ]

progStatus :: NonEmptyCursor Text -> Widget n
progStatus nec = hCenter $ border $ vBox $ concat
  [ map (drawText None) $ reverse $ NEC.nonEmptyCursorPrev nec
  , [drawText Highlight $ NEC.nonEmptyCursorCurrent nec]
  , map (drawText None) $ NEC.nonEmptyCursorNext nec
  ]

data Highlight = None | Highlight

drawText :: Highlight -> Text -> Widget n
drawText h = drawItem h . T.unpack

drawItem :: Highlight -> String -> Widget n
drawItem Highlight = withAttr "selected" . str . prePrep
drawItem _         = str . prePrep

prePrep = addEllipses . take maxwidth . filter isntWhite
 where
  maxwidth = 28
  addEllipses xs | length xs >= maxwidth = xs ++ "..."
                 | otherwise       = xs

isntWhite :: Char -> Bool
isntWhite ' ' = False
isntWhite 'Ω' = False
isntWhite x   = True


  