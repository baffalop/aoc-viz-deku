module Main
  ( main
  )
  where

import Prelude

import Data.Compactable (compact)
import Data.Int (floor, toNumber)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=), (<:=>))
import Deku.Attributes (klass_, style)
import Deku.Control (text)
import Deku.Core (fixed)
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
    pos :: Event Coord
    pos = fold add origin (compact $ vectorFromKey <$> Key.down)

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
          klass_ $ containerKlass <> " flex-1 flex items-center justify-center"
        [ D.div
            Alt.do
              klass_ "rounded-full border border-red-400 bg-red-500/40 w-6 h-6 transition-transform duration-200"
              style $ pos <#> \{ x, y } ->
                let p = toNumber >>> (_ * 1.5) in
                i "transform: translate("(p x)"rem, "(p y)"rem);"
          []
        ]
    ]

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
