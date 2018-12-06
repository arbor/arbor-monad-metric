module Arbor.Monad.Metric.Api.String
  ( Counter
  , Gauge
  , Tag

  , tags
  , tag
  , counter
  , gauge
  ) where

import Arbor.Monad.Metric.Type (Counter, Gauge, Tag)
import Data.Bifunctor
import Data.Set                (Set)

import qualified Arbor.Monad.Metric.Api.Text as T
import qualified Data.Text                   as T

counter :: String -> Counter
counter = T.counter . T.pack
{-# INLINE counter #-}

gauge :: String -> Gauge
gauge = T.gauge . T.pack
{-# INLINE gauge #-}

tag :: String -> String -> Tag
tag name value = T.tag (T.pack name) (T.pack value)
{-# INLINE tag #-}

tags :: [(String, String)] -> Set Tag
tags = T.tags . fmap (bimap T.pack T.pack)
{-# INLINE tags #-}
