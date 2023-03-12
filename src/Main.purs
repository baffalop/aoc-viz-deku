module Main
  ( main
  )
  where

import Prelude

import Data.Int (floor, toNumber)
import Data.Interpolate (i)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=), (<:=>))
import Deku.Attributes (klass_, style)
import Deku.Control (text)
import Deku.Core (fixed)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useRef, useState)
import Deku.Listeners (keyDown_, slider_)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import QualifiedDo.Alt as Alt
import Web.UIEvent.KeyboardEvent as Key

type Coord = { x :: Int, y :: Int }

origin :: Coord
origin = { x: 0, y: 0 }

main :: Effect Unit
main = runInBody Deku.do
  setN /\ n <- useState 10
  setPos /\ pos <- useState origin
  posRef <- useRef origin pos

  let
    moveX :: Int -> Effect Unit
    moveX dx = do
      currentPos <- posRef
      setPos $ currentPos { x = currentPos.x + dx }

    moveY :: Int -> Effect Unit
    moveY dy = do
      currentPos <- posRef
      setPos $ currentPos { y = currentPos.y + dy }

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
          D.Tabindex !:= "0"
          keyDown_ $ Key.key >>> case _ of
            "ArrowLeft" -> moveX (-1)
            "ArrowRight" -> moveX 1
            "ArrowUp" -> moveY (-1)
            "ArrowDown" -> moveY 1
            _ -> pure unit

        [ D.div
            Alt.do
              klass_ "rounded-full border border-red-400 bg-red-500/40 w-6 h-6"
              style $ pos <#> \{ x, y } ->
                let p x = toNumber x * 1.5 in
                i "translate: " (p x) "rem " (p y) "rem;"
          []
        ]
    ]

containerKlass :: String
containerKlass = "p-4 bg-slate-700"
