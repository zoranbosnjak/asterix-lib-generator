module Asterix.Common where

import           Data.Text.Lazy.Builder (Builder)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting as F

import           Asterix.Indent

-- | The same as 'line $ bformat (formating) arg1 arg2 ...'
fmt :: Format (BlockM Builder ()) a -> a
fmt m = runFormat m line

-- | Escape text (single quotes).
escaped :: Text -> Text
escaped s
    | s == "''" = s
    | otherwise = "'" <> T.replace "'" "\\'" s <> "'"

