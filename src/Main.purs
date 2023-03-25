module Main
  ( main
  )
  where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Apply (lift2)
import Data.Array as Array
import Data.Compactable (compact)
import Data.Filterable (filter, filterMap)
import Data.Int (toNumber, trunc)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Number (abs, sqrt, floor, ceil)
import Data.Ord (signum)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (!:=), (<:=>))
import Deku.Attributes (klass, klass_, style)
import Deku.Control (text, text_)
import Deku.Core (Domable, Nut)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useEffect, useMemoized, useState, useState')
import Deku.Listeners (click, click_, slider_)
import Deku.Pursx ((~~))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (Event, fold, gate, withLast)
import FRP.Event.Class ((*|>), (<|*))
import FRP.Event.Keyboard as Key
import QualifiedDo.Alt as Alt
import Type.Proxy (Proxy(..))
import Web.Event.Event (stopPropagation)

type Hook a = forall lock payload. (a -> Domable lock payload) -> Domable lock payload

type Point = { x :: Int, y :: Int }
type Vec = { x :: Number, y :: Number }
type Segment = { head :: Point, tail :: Point }

origin :: Point
origin = { x: 0, y: 0 }

initLength :: Int
initLength = 1

maxLength :: Int
maxLength = 30

main :: Effect Unit
main = runInBody Deku.do
  setLength /\ length <- useState initLength
  setGrowMode /\ grow <- useState true

  inc /\ lengthInc'd <- useState'
  dec /\ lengthDec'd <- useState'
  useEffect (lengthInc'd *|> length # filter (_ < maxLength)) $ setLength <<< (_ + 1)
  useEffect (lengthDec'd *|> length # filter (_ > 1))         $ setLength <<< (_ - 1)
  let incLength /\ decLength = inc unit /\ dec unit

  useEffect (filter (_ == "KeyA") Key.down) $ const incLength
  useEffect (filter (_ == "KeyS") Key.down) $ const decLength
  useEffect (filter (_ == "KeyG") Key.down *|> grow) $ setGrowMode <<< not

  let
    head :: Event Point
    head = pure origin <|> fold add origin (compact $ vectorFromKey <$> Key.down)

    descriptionPanel = Proxy :: Proxy """
<div class="w-56 space-y-3 row-span-full">
  <h1 class="font-bold text-4xl text-red-400 italic mb-6 tracking-wider">Rope/snake</h1>
  <p>This is a visualisation of the Rope Bridge puzzle from
    <a href="https://adventofcode.com/2022/day/9">Advent of Code 2022 day 9</a>.</p>
  <p>I used it as a playground to explore <a href="https://purescript-deku.netlify.app/">Deku</a>,
    a VDOMless Purescript framework based on Functional Reactive Programming.</p>
  <p>View the <a href="https://github.com/baffalop/aoc-viz-deku">source</a>.</p>
</div>"""

  rope :: Array (Event (Maybe Segment)) <- makeRope head length grow incLength

  D.div
    (klass_ "bg-slate-800 text-slate-100 p-8 h-screen grid gap-8 grid-rows-[auto_1fr] grid-cols-[auto_1fr]")
    [ descriptionPanel ~~ {}
    , D.div (klass_ "flex gap-6 justify-start items-stretch")
        [ D.div (klass_ controlsKlass)
            [ D.label (D.For !:= "length" <|> klass_ labelKlass) [text_ "Length"]
            , D.div (klass_ "flex gap-4 items-center")
                [ D.button (klass_ buttonKlass <|> click_ decLength) [text_ "-1"]
                , D.input
                    Alt.do
                      slider_ $ setLength <<< trunc
                      D.Name !:= "length"
                      D.OnKeydown !:= cb stopPropagation
                      D.Value <:=> show <$> length
                      D.Step !:= "1"
                      D.Min !:= "1"
                      D.Max !:= show maxLength
                  []
                , D.button (klass_ buttonKlass <|> click_ incLength) [text_ "+1"]
                , D.span (klass_ "w-4") [text $ show <$> length]
                ]
            ]
        , D.div (klass_ controlsKlass)
            [ D.label (D.For !:= "grow" <|> klass_ labelKlass) [text_ "Grow mode"]
            , D.input
                Alt.do
                  D.Xtype !:= "checkbox"
                  D.Name !:= "grow"
                  D.Checked <:=> show <$> grow
                  click $ setGrowMode <<< not <$> grow
                  klass_ "switch"
                []
            ]
        ]
    , D.div (klass_ $ containerKlass <> " flex-1 relative")
        $ rope <#> ropeSegment "border-red-400 bg-red-500/40"
    ]
  where
    buttonKlass = "py-0.5 px-2 rounded border border-teal-400 text-teal-400 text-sm font-medium bg-teal-500/10 hover:bg-teal-500/25"
    containerKlass = "p-4 pt-2 bg-slate-700 rounded-lg border-2 border-slate-600"
    controlsKlass = containerKlass <> " max-w-max space-y-2.5"
    labelKlass = "font-bold italic text-slate-300 block"

ropeSegment :: String -> Event (Maybe Segment) -> Nut
ropeSegment klasses segment =
  D.div
    Alt.do
      klass $ segment <#> isNothing >>> \hidden ->
        (if hidden then "hidden" else "absolute")
        <> " rounded-full border transition-all duration-200 left-1/2 top-1/2 "
        <> klasses

      style $ ((/\) <$> segment' <*> turnsState) <#> \({ head, tail } /\ turns) ->
        let
          d = delta tail head
          width = (sqrt (d.x * d.x + d.y * d.y) + 1.0) * weight
        in
        i "width: "width"rem; \
          \height: "weight"rem; \
          \transform: \
            \translate("(trans tail.x)"rem, "(trans tail.y)"rem) \
            \rotate("turns"turn); \
          \transform-origin: "halfWeight"rem "halfWeight"rem;"

    []
  where
    weight = 1.5
    halfWeight = weight / 2.0

    trans :: Int -> Number
    trans v = (toNumber v * weight) - halfWeight

    segment' :: Event Segment
    segment' = compact segment

    turnsState :: Event Number
    turnsState =
      fold (\last cur -> closest last (floor last + cur) (ceil last + cur)) 0.0
        $ turnsIn <$> segment'

    turnsIn :: Segment -> Number
    turnsIn { head, tail } = case delta tail head of
      { x: -1.0, y: 0.0 } -> 0.5
      { x: dx, y: dy } -> (dx - 2.0) * (-0.125) * signum dy

makeRope :: Event Point -> Event Int -> Event Boolean -> Effect Unit -> Hook (Array (Event (Maybe Segment)))
makeRope initialHead length grow incLength f = unfold 1 (Just <$> initialHead) []
  where
    unfold i head rope
      | i > maxLength = f rope
      | otherwise = Deku.do
        let
          canGrow :: Event Boolean
          canGrow = (&&) <$> grow <*> ((i > _) <$> length)

          headMovedFrom :: Event Point
          headMovedFrom = withLast head # filterMap case _ of
            { last: Just (Just last), now: Just now } | last /= now -> Just last
            _ -> Nothing

          growth :: Event Point
          growth = gate canGrow headMovedFrom

        useEffect growth $ const incLength

        tail <- useMemoized $ dedup $ fold maybeFollow Nothing ado
          len <- length
          head' <- compact head
          growth' <- Alt.do
            pure Nothing
            Just <$> growth
            Nothing <$ filter not canGrow
          in maybeIf (i <= len) head' <|> growth'

        segment <- useMemoized $ lift2 { head: _, tail: _ } <$> head <*> tail

        unfold (i + 1) tail $ Array.snoc rope segment

maybeFollow :: Maybe Point -> Maybe Point -> Maybe Point
maybeFollow tailM headM = do
  head <- headM
  Just $ fromMaybe head do
    tail <- tailM
    Just $ fromMaybe tail $ tail `follow` head

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

closest :: Number -> Number -> Number -> Number
closest to x y = case compare dx dy of
  LT -> x
  GT -> y
  EQ -> if abs x < abs y then x else y
  where
    (dx /\ dy) = abs (to - x) /\ abs (to - y)

maybeIf :: forall a. Boolean -> a -> Maybe a
maybeIf = if _ then Just else const Nothing

vectorFromKey :: String -> Maybe Point
vectorFromKey = case _ of
  "ArrowUp" -> Just { x: 0, y: -1 }
  "ArrowDown" -> Just { x: 0, y: 1 }
  "ArrowLeft" -> Just { x: -1, y: 0 }
  "ArrowRight" -> Just { x: 1, y: 0 }
  _ -> Nothing

dedup :: forall a. Eq a => Event a -> Event a
dedup event =
  event <|* filter (\{ last, now } -> Just now /= last) (withLast event)
