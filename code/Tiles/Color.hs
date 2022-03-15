{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tiles.Color where

import Codec.Picture
import Data.Word

type Color = PixelRGBA8

instance Semigroup Color where
  (<>) = _over

instance Monoid Color where
  mempty = rgba 0 0 0 0

rgba :: Double -> Double -> Double -> Double -> Color
rgba r g b a =
  PixelRGBA8
    (bounded r)
    (bounded g)
    (bounded b)
    (bounded a)
  where
    bounded :: Double -> Word8
    bounded x = round $ x * fromIntegral (maxBound @Word8)

pattern Color :: Double -> Double -> Double -> Double -> Color
pattern Color r g b a <-
  PixelRGBA8
    (fromIntegral -> (/255) -> r)
    (fromIntegral -> (/255) -> g)
    (fromIntegral -> (/255) -> b)
    (fromIntegral -> (/255) -> a)
  where
    Color = rgba
{-# COMPLETE Color #-}

invert :: Color -> Color
invert (Color r g b a) = Color (1 - r) (1 - g) (1 - b) a

_over :: Color -> Color -> Color
_over (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  let aa = norm a1
      ab = norm a2
      a' = aa + ab * (1 - aa)
      norm :: Word8 -> Double
      norm x = fromIntegral x / 255
      unnorm :: Double -> Word8
      unnorm x = round $ x * 255
      f :: Word8 -> Word8 -> Word8
      f a b = unnorm $ (norm a * aa + norm b * ab * (1 - aa)) / a'
   in PixelRGBA8 (f r1 r2) (f g1 g2) (f b1 b2) (unnorm a')

_mask :: Color -> Color -> Color
_mask (PixelRGBA8 _ _ _ a) (PixelRGBA8 r g b _) = PixelRGBA8 r g b a
