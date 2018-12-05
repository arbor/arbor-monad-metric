module Arbor.Monad.Metric.Api.Text
  ( Counter
  , Gauge
  , Tag

  , tags
  , tag
  , counter
  , gauge
  ) where

import Arbor.Monad.Metric.Type (Counter, Gauge, Tag)
import Data.Set                (Set)
import Data.Text               (Text)

import qualified Arbor.Monad.Metric.Type as Z
import qualified Data.Set                as S

counter :: Text -> Counter
counter name = Z.Counter name S.empty
{-# INLINE counter #-}

gauge :: Text -> Gauge
gauge name = Z.Gauge name S.empty
{-# INLINE gauge #-}

tag :: Text -> Text -> Tag
tag = Z.Tag
{-# INLINE tag #-}

tags :: [(Text, Text)] -> Set Tag
tags nvs = S.fromList (uncurry tag <$> nvs)
{-# INLINE tags #-}
