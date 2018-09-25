module Data.Text.Serialize where

import Data.Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Serialize

{-
Just the serialize instance for Text
-}

instance Serialize Text where
    put txt = put $ encodeUtf8 txt
    get     = fmap decodeUtf8 get
