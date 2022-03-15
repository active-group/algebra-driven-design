{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tiles.Exercise where

import Codec.Picture.Png
import Codec.Picture.Types
import Control.Applicative hiding (empty)
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Foldable
import Data.Functor.Compose
import Data.List (transpose)
import Data.Word
import QuickSpec
import System.IO.Unsafe
import Test.QuickCheck hiding (label, sample)

import Tiles.Color

