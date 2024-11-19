module Language.Lambda.Utils (
    tshow,
) where

import qualified Data.Text as Text

tshow :: (Show a) => a -> Text.Text
tshow = Text.pack . show
