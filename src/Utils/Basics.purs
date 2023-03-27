module Utils.Basics where

import Prelude

import Data.Array (snoc)
import Data.Array as Array
import Data.Int (rem)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Data.Tuple.Nested ((/\))

closest :: Number -> Number -> Number -> Number
closest to x y = case compare dx dy of
  LT -> x
  GT -> y
  EQ -> if abs x < abs y then x else y
  where
    (dx /\ dy) = abs (to - x) /\ abs (to - y)

maybeIf :: forall a. Boolean -> a -> Maybe a
maybeIf = if _ then Just else const Nothing

cycleTo :: forall a. Int -> Array a -> Array a
cycleTo length ar =
  join $ Array.replicate count ar `snoc` Array.take leftover ar
  where
    alen = Array.length ar
    count = length / (alen - 1)
    leftover = length `rem` alen
