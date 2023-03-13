module Main
  ( main
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Compactable (compact)
import Data.Int (floor, toNumber)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
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

type Coord = { x :: Int, y :: Int }

origin :: Coord
origin = { x: 0, y: 0 }

main :: Effect Unit
main = runInBody Deku.do
  setN /\ n <- useState 10

  let
    head :: Event Coord
    head = pure origin <|> fold add origin (compact $ vectorFromKey <$> Key.down)

    tail :: Event Coord
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
        [ point head "border-red-400 bg-red-500/40"
        , point tail "border-blue-400 bg-blue-500/40 delay-[50ms]"
        ]
    ]

point :: Event Coord -> String -> Nut
point coord klass =
  D.div
    Alt.do
      klass_ $
        "rounded-full border w-6 h-6 transition-transform duration-200 absolute left-1/2 top-1/2 "
        <> klass
      style $ coord <#> \{ x, y } ->
        i "transform: translate(calc("(p x)"rem - 50%), calc("(p y)"rem - 50%));"
    []
  where
    p = toNumber >>> (_ * 1.5)

follow :: Coord -> Coord -> Coord
follow follower target =
  add follower $ if shouldFollow then { x: signum vx, y: signum vy } else origin
  where
    shouldFollow = abs (toNumber vx) > 1.0 || abs (toNumber vy) > 1.0
    vx = target.x - follower.x
    vy = target.y - follower.y

containerKlass :: String
containerKlass = "p-4 bg-slate-700"

add :: Coord -> Coord -> Coord
add c1 c2 =
  { x: c1.x + c2.x
  , y: c1.y + c2.y
  }

vectorFromKey :: String -> Maybe Coord
vectorFromKey = case _ of
  "ArrowUp" -> Just { x: 0, y: -1 }
  "ArrowDown" -> Just { x: 0, y: 1 }
  "ArrowLeft" -> Just { x: -1, y: 0 }
  "ArrowRight" -> Just { x: 1, y: 0 }
  _ -> Nothing
