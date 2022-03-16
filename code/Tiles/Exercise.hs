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

-- import Tiles.Color
import Prelude hiding (Real)
import Data.Bits (Bits(xor))




{-
Java:
interface Tile {
    Tile clockwise();
}

class TileFactory {
    Tile clockwise(Tile tile) {
        if (tile instanceof Clockwise) {
            Clockwise clockwise = (Clockwise) tile;
            if (clockwise.tile instanceof Clockwise)
        }
    }
}

class Circle implements Tile {
    Point center;
    Real radius;
}
class Rectangle implements Tile { ... }

class Clockwise implements Tile {
    Tile tile;
}

class FlipH implements Tile {
    Tile tile;
}

class Beside implements Tiile {
    Tile tile1;
    Tile tile2;
}

-}

-- "leerer Datentyp"
-- Mathematik: eine Menge namens Tile
-- Mathematik-Studium: N, R
-- initiale Algebra
-- Java: Tile<Color>
-- Tile Kandidat für einen Funktor
-- Können wir fmap-Operation definieren?
-- Antwort: ja
data Tile color =
    Circle Point Real
  -- räumlicher Anordnung
  | Clockwise (Tile color)
  | FlipH (Tile color)
  | Beside (Tile color) (Tile color)

  -- Farben
  | Behind (Tile color) (Tile color)
--  | Color Real Real Real Real
  | Monochrome color
  deriving Show

{-
Typen mit Generic / Typparameter:

List<T>

Stream<T>
<R> Stream<R>   map(Function<T, R> mapper)

Optional<T>
<R> Optional<T> map(Function<T, R> mapper)

Funktor

-}

{-
static Tile circle(Point point, Real radius)
-}

data Point = Point Real Real
  deriving Show

type Real = Double 

-- OOP: macht eine Factory
circle :: Point -> Real -> Tile color 
circle point radius = Circle point radius

rectangle :: Point -> Real -> Real -> Tile color
rectangle lowerLeftCorner width height = undefined

-- im Uhrzeigersinn um 90° rotieren
-- "smart constructor"
clockwise :: Tile color -> Tile color
clockwise (Clockwise (Clockwise (Clockwise tile))) = tile
clockwise tile = Clockwise tile

cw :: Tile color -> Tile color
cw = clockwise

-- Identität:
-- clockwise tile = tile

-- Identität
identity :: a -> a
identity x = x 

{-
fmap :: (a -> b) -> f a -> f b
fmap identity list = list
o (fmap f) (fmap g) = fmap (o f g)
-}

o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ x -> f (g x)

{-
Function<A, C> o(Function<B, C> f, Function<A, B> g) {
    return x -> f(g(x)) 
}

-}


{-
(I)
cw/cw/cw/cw
∀t ∈ Tile :
clockwise (clockwise (clockwise (clockwise t))) = t

Bedeutet aber auch:
cw = clockwise

cw (cw (cw (cw (cw (cw (cw t)))))) =
cw (cw (cw t))
ergibt sich aus der Gleichung oben
-}

-- gegen den Uhrzeigersinn um 90° drehen
counterClockwise :: Tile color -> Tile color
counterClockwise tile = cw (cw (cw tile))

ccw = counterClockwise

{-
∀t :: Tile :
ccw (ccw (ccw (ccw t))) = t

(II)
cw/ccw
∀a :: Tile :
cw (ccw a) = a

(III)
ccw/cw
∀a :: Tile :
ccw (cw a) = a

-- Allgemeinwissen
ccw t = cw (cw (cw t))


-- hergeleitet aus (I) und (III)
ccw t = ccw (cw (cw (cw (cw t))))  (I)
      = cw (cw (cw t))             (III) mit a = (cw (cw (cw t)))

(III)
ccw (cw (cw (cw (cw t)))) = cw (cw (cw t))

-}

-- horizontal spiegeln
flipH :: Tile color -> Tile color
flipH (FlipH tile) = tile
flipH tile = FlipH tile

{-
(IV)
∀t :: Tile :
flipH (flipH t) = t

-- eine Möglichkeit - konsistent mit (IV)
flipH t = cw (cw t)

-- abgeleitete Gleichung
flipH (flipH t) = cw (cw (cw (cw t)))

(V)
∀t :: Tile :
flipH (cw (cw (flipH (cw (cw t)))) = t


-}

-- vertikal spiegeln
flipV :: Tile color -> Tile color
flipV tile = flipH (cw (cw tile)) 

{-
(VI)
∀t :: Tile :
flipH t = flipV (cw (cw t))

-- günstiger:
flipV t = flipH (cw (cw t))

Brauchen wir das hier?
flipV (flipV t) = t

nein, ist abgeleitet:
flipV (flipV t) = (VI) flipH (cw (cw (flipV t))
                = (VI) flipH (cw (cw (flipH (cw (cw t)))))
                = (V)  t

-}

-- "closure of operations"

-- 2 Bilder nebeneinander
beside :: Tile color -> Tile color -> Tile color
beside tile1 tile2 = Beside tile1 tile2

{-
(VII)
∀t1, t2 :: Tile :
flipH (beside t1 t2) = beside (flipH t2) (flipH t1)
-}

-- 2 Bilder übereinander
above :: Tile color -> Tile color -> Tile color
above tile1 tile2 = ccw (beside (cw tile1) (cw tile2))

{-
(VIII)
∀t1, t2 :: Tile :
flipV (above t1 t2) = above (flipV t2) (flipV t1)

(IX)
above t1 t2 = ccw (beside (cw t1) (cw t2))
-}

-- zwei Bilder hintereinander legen
behind :: Tile color -> Tile color -> Tile color
behind tile1 tile2 = Behind tile1 tile2

-- Bild in einer Farbe, mit Transparenz
color :: Real -> Real -> Real -> Real -> Tile color
-- jeweils Werte zwischen 0 und 1, 1 ist undurchsichtig
color red green blue alpha = Color red green blue alpha

{-
Annahme: "Tile" ist immer quadratisch

∀r, g, b a :: Real[0,1] :
cw (color r g b a) = color r g b a

∀r, g, b:: Real[0,1], t :: Tile :
behind t (color r g b 1) = color r g b 1

"Algebra von Bildern"
-}

{-
a + (b + c) = (a + b) + c

-}

{-
Terme in der Algebra stehen i.d.R. für Objekte
"Beobachtungen" / "Observations"
-}

data Color

-- "Liste von Listen"
-- Liste von Zeilen
type Observation color = [[color]]

tileSize = 10

observation :: Tile color -> Observation color
observation (Circle center radius) = undefined
observation (FlipH tile) =
    -- jede Zeile umdrehen
     map reverse (observation tile)
{-
observation (Behind tile1 tile2) =
 Diese beiden Bilder werden pixelweise kombiniert
 - observation tile1 
 - observation tile2
benötigen: Kombinationsoperation auf Color
behindColor :: Color -> Color -> Color

Die Assoziativität von behind folgt aus 
der Assoziativität von behindColor
-}
-- usw.

{-

behind a (behind b c) = behind (behind a b) c

MEINT EIGENTLICH:

observe (behind a (behind b c)) = 
    observe (behind (behind a b) c)
-}













