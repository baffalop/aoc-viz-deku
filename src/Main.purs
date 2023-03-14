module Main
  ( main
  , follow
  )
  where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array ((!!), (..), (:))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Int (floor, toNumber, trunc)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..))
import Data.Number (abs, sqrt)
import Data.Ord (signum)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=), (<:=>))
import Deku.Attributes (klass_, style)
import Deku.Control (text)
import Deku.Core (fixed, Nut)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Listeners (slider_)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (Event, fold)
import FRP.Event.Keyboard as Key
import QualifiedDo.Alt as Alt

type Point = { x :: Int, y :: Int }
type Vec = { x :: Number, y :: Number }

type Segment = { head :: Point, tail :: Point }

origin :: Point
origin = { x: 0, y: 0 }

windowSize :: Int
windowSize = 5

main :: Effect Unit
main = runInBody Deku.do
  setN /\ n <- useState 10

  let
    head :: Event Point
    head = pure origin <|> fold add origin (compact $ vectorFromKey <$> Key.down)

    tail :: Event (Array Point)
    tail = fold trailBehind (Array.replicate windowSize origin) head

    rope :: Event (Array Point)
    rope = (:) <$> head <*> tail

  fixed
    [ D.div
        Alt.do
          klass_ $ containerKlass <> " space-x-4 max-w-max"
        [ D.input
            Alt.do
              slider_ $ setN <<< floor
              klass_ "cursor-pointer"
              D.Value <:=> show <$> n
              D.Step !:= "1"
              D.Min !:= "1"
              D.Max !:= "20"
          []
        , D.span_ [text $ show <$> n]
        ]
    , D.div
        Alt.do
          klass_ $ containerKlass <> " flex-1 relative"
        $ 0 .. windowSize <#> \i ->
            ropePoint "border-red-400 bg-red-500/40" $ compact $ rope <#> (_ !! i)
    ]
  where
    containerKlass = "p-4 bg-slate-700"

ropePoint :: String -> Event Point -> Nut
ropePoint klass point = ropeSegment klass $ point <#> \p -> { head: p, tail: p }

ropeSegment :: String -> Event Segment -> Nut
ropeSegment klass points =
  D.div
    Alt.do
      klass_ $
        "rounded-full border transition-all duration-200 absolute left-1/2 top-1/2 "
        <> klass
      style $ transformFrom <$> points
    []
  where
    transformFrom { head, tail } =
      let
        d = delta tail head
        width = (sqrt (d.x * d.x + d.y * d.y) + 1.0) * weight
        turns = case d.x, d.y of
          (-1.0), 0.0 -> 0.5
          dx, dy -> (dx - 2.0) * (-0.125) * signum dy
      in
      i "width: "width"rem; \
        \height: "weight"rem; \
        \transform: \
          \translate("(p tail.x)"rem, "(p tail.y)"rem) \
          \rotate("turns"turn); \
        \transform-origin: "halfWeight"rem "halfWeight"rem;"

    p v = (toNumber v * 1.5) - 0.75
    weight = 1.5
    halfWeight = weight / 2.0

trailBehind :: Array Point -> Point -> Array Point
trailBehind trail head = case Array.uncons trail of
  Nothing -> []
  Just { head: neck, tail } -> case neck `follow` head of
    Nothing -> trail
    Just newNeck -> newNeck : trailBehind tail newNeck

follow :: Point -> Point -> Maybe Point
follow follower target = do
  guard $ abs d.x > 1.0 || abs d.y > 1.0
  pure $ follower `add`
    { x: trunc $ signum d.x
    , y: trunc $ signum d.y
    }
  where
    d = delta follower target

add :: Point -> Point -> Point
add c1 c2 =
  { x: c1.x + c2.x
  , y: c1.y + c2.y
  }

delta :: Point -> Point -> Vec
delta from to =
  { x: toNumber $ to.x - from.x
  , y: toNumber $ to.y - from.y
  }

vectorFromKey :: String -> Maybe Point
vectorFromKey = case _ of
  "ArrowUp" -> Just { x: 0, y: -1 }
  "ArrowDown" -> Just { x: 0, y: 1 }
  "ArrowLeft" -> Just { x: -1, y: 0 }
  "ArrowRight" -> Just { x: 1, y: 0 }
  _ -> Nothing
