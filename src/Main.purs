module Main
  ( main
  )
  where

import Prelude hiding (add)

import Control.Alt ((<|>))
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

origin :: Point
origin = { x: 0, y: 0 }

main :: Effect Unit
main = runInBody Deku.do
  setN /\ n <- useState 10

  let
    head :: Event Point
    head = pure origin <|> fold add origin (compact $ vectorFromKey <$> Key.down)

    tail :: Event Point
    tail = fold follow origin head

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
          klass_ $ containerKlass <> " flex-1 flex items-center justify-center relative"
        [ link ({ head: _, tail: _ } <$> head <*> tail) "border-red-400 bg-red-500/40"
        ]
    ]
  where
    containerKlass = "p-4 bg-slate-700"

link :: Event { head :: Point, tail :: Point } -> String -> Nut
link coords klass =
  D.div
    Alt.do
      klass_ $
        "rounded-full border w-6 h-6 transition-all duration-200 absolute left-1/2 top-1/2 "
        <> klass
      style $ transformFrom <$> coords
    []
  where
    transformFrom { head, tail } =
      let
        d = delta tail head
        width = (sqrt (d.x * d.x + d.y * d.y) + 1.0) * 1.5
        turns = case d.x, d.y of
          (-1.0), 0.0 -> 0.5
          dx, dy -> (dx - 2.0) * (-0.125) * signum dy
      in
      i "width: "width"rem; \
        \transform: \
          \translate("(p tail.x)"rem, "(p tail.y)"rem) \
          \rotate("turns"turn); \
        \transform-origin: 0.75rem 0.75rem;"

    p v = (toNumber v * 1.5) - 0.75

follow :: Point -> Point -> Point
follow follower target =
  if abs d.x > 1.0 || abs d.y > 1.0
    then follower `add`
      { x: trunc $ signum d.x
      , y: trunc $ signum d.y
      }
    else follower
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
