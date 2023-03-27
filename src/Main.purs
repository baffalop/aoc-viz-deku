module Main
  ( main
  )
  where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Apply (lift2)
import Data.Array (snoc, zipWith, (:))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Filterable (filter, filterMap)
import Data.Int (rem, toNumber, trunc)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (abs, sqrt, floor, ceil)
import Data.Ord (signum)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
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
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class ((*|>), (<*|>), (<|*))
import FRP.Event.Keyboard as Key
import FRP.Event.Time (interval)
import Heterogeneous.Mapping (hmap)
import QualifiedDo.Alt as Alt
import Type.Proxy (Proxy(..))
import Web.CSSOM.MouseEvent (offsetX, offsetY) as Mouse
import Web.CSSOMView.Window (devicePixelRatio)
import Web.DOM.Element (getBoundingClientRect)
import Web.DOM.Element as Element
import Web.Event.Event (currentTarget, stopPropagation)
import Web.Event.Event as WebEvent
import Web.HTML (window)
import Web.UIEvent.MouseEvent (fromEvent) as Mouse

type Hook a = forall lock payload. (a -> Domable lock payload) -> Domable lock payload

type Point = { x :: Int, y :: Int }
type Vec = { dx :: Number, dy :: Number }
type Segment = { head :: Point, tail :: Point }

origin :: Point
origin = { x: 0, y: 0 }

initLength :: Int
initLength = 1

maxLength :: Int
maxLength = 30

segmentWeightPx :: Number
segmentWeightPx = 24.0

segmentHalfWeightPx :: Number
segmentHalfWeightPx = segmentWeightPx / 2.0

descriptionPanel :: Proxy """
<div class="w-56 space-y-3 row-span-full">
  <h1 class="font-bold text-4xl text-red-400 italic mb-6 tracking-wider">Rope/snake</h1>
  <p>This is a visualisation of the Rope Bridge puzzle from
    <a href="https://adventofcode.com/2022/day/9">Advent of Code 2022 day 9</a>.</p>
  <p>I used it as a playground to explore <a href="https://purescript-deku.netlify.app/">Deku</a>,
    a VDOMless Purescript framework based on Functional Reactive Programming.</p>
  <p>View the <a href="https://github.com/baffalop/aoc-viz-deku">source</a>.</p>
</div>""" -- """
descriptionPanel = Proxy

main :: Effect Unit
main = runInBody Deku.do
  setLength /\ length <- useState initLength
  setGrowMode /\ grow <- useState true
  setMotor /\ motor <- useState true
  setTarget /\ target <- useState (Nothing :: Maybe Point)

  inc /\ lengthInc'd <- useState'
  dec /\ lengthDec'd <- useState'
  useEffect (lengthInc'd *|> length # filter (_ < maxLength)) $ setLength <<< (_ + 1)
  useEffect (lengthDec'd *|> length # filter (_ > 1))         $ setLength <<< (_ - 1)
  let incLength /\ decLength = inc unit /\ dec unit

  useEffect (filter (_ == "KeyA") Key.down) $ const incLength
  useEffect (filter (_ == "KeyS") Key.down) $ const decLength
  useEffect (filter (_ == "KeyG") Key.down *|> grow) $ setGrowMode <<< not
  useEffect (filter (_ == "KeyM") Key.down *|> motor) $ setMotor <<< not

  let
    keyControl :: Event Point
    keyControl = filterMap vectorFromKey Key.down

    head :: Event Point
    head = pure origin <|> fold add origin Alt.do
      keyControl
      keyControl <|* gate motor (interval 200)

    targetEl :: Nut
    targetEl = ropeSegment "border-yellow-500 bg-yellow-500/40" $ target <#> map \t -> { head: t, tail: t }

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
        , switch "grow" "Grow as I move" grow setGrowMode
        , switch "motor" "Motor" motor setMotor
        ]
    , D.div
        Alt.do
          klass_ $ containerKlass <> " flex-1 relative cursor-pointer"
          click_ $ cb $ setTarget <=< mousePoint
        $ targetEl : (zipWith ropeSegment (cycleTo maxLength rainbow) rope)
    ]
  where
    buttonKlass = "py-0.5 px-2 rounded border border-teal-400 text-teal-400 text-sm font-medium bg-teal-500/10 hover:bg-teal-500/25"

switch :: String -> String -> Event Boolean -> (Boolean -> Effect Unit) -> Nut
switch name label state setState =
  D.div (klass_ controlsKlass)
    [ D.label
        Alt.do
          D.For !:= name
          klass_ labelKlass
        [text_ label]
    , D.input
        Alt.do
          D.Xtype !:= "checkbox"
          D.Name !:= name
          D.Checked <:=> show <$> state
          click $ setState <<< not <$> state
          klass_ "switch"
        []
    ]

ropeSegment :: String -> Event (Maybe Segment) -> Nut
ropeSegment klasses segment = Deku.do
  transitionend /\ transitionKlass <- transition (isJust <$> segment)
    { gone: "hidden"
    , here: "absolute"
    , enterFrom: "!w-0 !h-0"
    , enterTo: "w-6"
    , leaveFrom: "w-6"
    , leaveTo: "!w-0 !h-0"
    }

  D.div
    Alt.do
      D.OnTransitionend !:= transitionend
      klass $ transitionKlass <#> i klasses" rounded-full border transition-all duration-200 left-1/2 top-1/2 "

      style ado
        { head, tail } <- segment'
        turns <- turnsState
        let
          { dx, dy } = delta tail head
          width = (sqrt (dx * dx + dy * dy) + 1.0) * segmentWeightPx
          halfWeightPctWidth = (segmentHalfWeightPx / width) * 100.0
        in
        i "width: "width"px; \
          \height: "segmentWeightPx"px; \
          \left: calc(50% + "(offset tail.x)"px); \
          \top: calc(50% + "(offset tail.y)"px); \
          \transform: \
            \translate(-"halfWeightPctWidth"%, -50%) \
            \rotate("turns"turn); \
          \transform-origin: "segmentHalfWeightPx"px "segmentHalfWeightPx"px;"

    []
  where
    offset :: Int -> Number
    offset v = (toNumber v * segmentWeightPx) - segmentHalfWeightPx

    segment' :: Event Segment
    segment' = compact segment

    turnsState :: Event Number
    turnsState =
      fold (\last cur -> closest last (floor last + cur) (ceil last + cur)) 0.0
        $ turnsIn <$> segment'

    turnsIn :: Segment -> Number
    turnsIn { head, tail } = case delta tail head of
      { dx: -1.0, dy: 0.0 } -> 0.5
      { dx, dy } -> (dx - 2.0) * (-0.125) * signum dy

labelKlass :: String
labelKlass = "font-bold italic text-slate-300 block"

controlsKlass :: String
controlsKlass = containerKlass <> " max-w-max space-y-2.5"

containerKlass :: String
containerKlass = "p-4 pt-2 bg-slate-700 rounded-lg border-2 border-slate-600"

type TransitionKlasses =
  { here :: String
  , gone :: String
  , enterFrom :: String
  , enterTo :: String
  , leaveFrom :: String
  , leaveTo :: String
  }

data TransitionState
  = BeforeEnter
  | Entering
  | AfterEnter
  | Here
  | BeforeLeave
  | Leaving
  | AfterLeave
  | Gone

transition :: Event Boolean -> TransitionKlasses -> Hook (Effect Unit /\ Event String)
transition exists klasses f = Deku.do
  setState /\ state <- useState Gone
  onTransitionend /\ transitionend <- useState'

  useEffect ((/\) <$> exists <*|> state) case _ of
    true /\ Here -> pure unit
    true /\ _ -> setState BeforeEnter
    false /\ Gone -> pure unit
    false /\ _ -> setState BeforeLeave

  useEffect (animationFrame *|> state) case _ of
    BeforeEnter -> setState Entering
    AfterEnter -> setState Here
    BeforeLeave -> setState Leaving
    AfterLeave -> setState Gone
    _ -> pure unit

  useEffect (transitionend *|> state) case _ of
    Entering -> setState Here
    Leaving -> setState Gone
    _ -> pure unit

  let
    klass = state <#> case _ of
      BeforeEnter -> i klasses.here" "klasses.enterFrom
      Entering    -> i klasses.here" "klasses.enterTo
      AfterEnter  -> klasses.here
      Here        -> klasses.here
      BeforeLeave -> i klasses.here" "klasses.leaveFrom
      Leaving     -> i klasses.here" "klasses.leaveTo
      AfterLeave  -> klasses.gone
      Gone        -> klasses.gone

  f $ onTransitionend unit /\ klass

makeRope :: Event Point -> Event Int -> Event Boolean -> Effect Unit -> Hook (Array (Event (Maybe Segment)))
makeRope initialHead length grow incLength f = unfold 1 (Just <$> initialHead) []
  where
    unfold i head rope
      | i > maxLength = f rope
      | otherwise = Deku.do
        let
          canGrow :: Event Boolean
          canGrow = ado
            growModeOn <- grow
            len <- length
            in growModeOn && i > len

          headMovedFrom :: Event Point
          headMovedFrom = withLast head # filterMap case _ of
            { last: Just (Just last), now: Just now } | last /= now -> Just last
            _ -> Nothing

          growth :: Event Point
          growth = gate canGrow headMovedFrom

          leader :: Event (Maybe Point)
          leader = ado
            len <- length
            head' <- compact head
            growth' <- Just <$> growth <|> Nothing <$ filter not canGrow
            in maybeIf (i <= len) head' <|> growth'

        useEffect growth $ const incLength

        tail <- useMemoized $ dedup $ fold maybeFollow Nothing leader
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
  guard $ abs dx > 1.0 || abs dy > 1.0
  pure $ add follower $ hmap (trunc <<< signum) { x: dx , y: dy }
  where
     { dx, dy } = delta follower target

add :: Point -> Point -> Point
add p1 p2 =
  { x: p1.x + p2.x
  , y: p1.y + p2.y
  }

delta :: Point -> Point -> Vec
delta from to = hmap toNumber
  { dx: to.x - from.x
  , dy: to.y - from.y
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

mousePoint :: WebEvent.Event -> Effect (Maybe Point)
mousePoint ev = do
  pixelRatio <- window >>= devicePixelRatio
  maybeRect <- traverse getBoundingClientRect $ Element.fromEventTarget =<< currentTarget ev
  pure do
    mouseEvent <- Mouse.fromEvent ev
    rect <- maybeRect
    pure $ hmap (trunc <<< (_ / (pixelRatio * segmentHalfWeightPx)))
      { x: toNumber (Mouse.offsetX mouseEvent) - (rect.width / 2.0)
      , y: toNumber (Mouse.offsetY mouseEvent) - (rect.height / 2.0)
      }

cycleTo :: forall a. Int -> Array a -> Array a
cycleTo length ar =
  join $ Array.replicate count ar `snoc` Array.take leftover ar
  where
    alen = Array.length ar
    count = length / (alen - 1)
    leftover = length `rem` alen

rainbow :: Array String
rainbow =
  [ "border-red-400 bg-red-500/40"
  , "border-pink-400 bg-pink-500/40"
  , "border-purple-400 bg-purple-500/40"
  , "border-indigo-400 bg-indigo-500/40"
  , "border-sky-400 bg-sky-500/40"
  , "border-teal-400 bg-teal-500/40"
  , "border-green-400 bg-green-500/40"
  , "border-yellow-400 bg-yellow-500/40"
  , "border-orange-400 bg-orange-500/40"
  ]
