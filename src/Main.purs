module Main
  ( main
  )
  where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Apply (lift2)
import Data.Array (zipWith, (:))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Filterable (filter, filterMap)
import Data.Int (toNumber, trunc)
import Data.Int as Int
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Number (abs, sqrt, floor)
import Data.Number as Number
import Data.Ord (signum)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Deku.Attribute (cb, (!:=), (<:=>))
import Deku.Attributes (klass, klass_, style)
import Deku.Control (text, text_, (<#~>))
import Deku.Control as DekuC
import Deku.Core (Domable, Nut)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useEffect, useHot, useMemoized, useState, useState')
import Deku.Listeners (click, click_, slider_)
import Deku.Pursx ((~~))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (Event, delay, fold, gate, withLast)
import FRP.Event.Class ((*|>))
import FRP.Event.Keyboard as Key
import FRP.Event.Time (interval)
import Heterogeneous.Mapping (hmap)
import QualifiedDo.Alt as Alt
import Type.Proxy (Proxy(..))
import Utils.Basics (maybeIf, closest, cycleTo)
import Utils.Deku (Hook, TransitionState(..), textInput_, transition)
import Utils.FRP (dedup)
import Web.CSSOM.MouseEvent (offsetX, offsetY) as Mouse
import Web.CSSOMView.Window (devicePixelRatio)
import Web.DOM.Element (getBoundingClientRect)
import Web.DOM.Element as El
import Web.Event.Event (currentTarget, preventDefault, stopPropagation)
import Web.Event.Event as WebEvent
import Web.HTML (window)
import Web.UIEvent.MouseEvent (fromEvent) as Mouse
import Deku.DOM.Elt.Textarea (textarea) as D
import Data.Either (Either(..))
import Parsing (ParseError, parseErrorMessage, runParser)
import Parsing (fail) as P

type Point = { x :: Int, y :: Int }
type Delta = { x :: Number, y :: Number }
type Segment = { head :: Point, tail :: Point }

data Motion
  = Vector Point
  | Target Point

origin :: Point
origin = { x: 0, y: 0 }

initLength :: Int
initLength = 1

maxLength :: Int
maxLength = 30

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
  growState@(setGrowMode /\ grow) <- useState true
  motorState@(setMotor /\ motor) <- useState false
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

  setInstructions /\ instructions <- useState ([] :: Array Point)

  let
    clock :: Event Unit
    clock = unit <$ interval 100

    keyControl :: Event Motion
    keyControl = filterMap vectorFromKey Key.down

    head :: Event Point
    head = pure origin <|> fold applyMotion origin Alt.do
      keyControl
      gate ((&&) <$> motor <*> (target <#> isNothing)) clock *|> keyControl
      gate (target <#> isJust) clock *|> (Target <$> compact target)

    targetEl :: Nut
    targetEl = ropeSegment "border-yellow-500 bg-yellow-500/40" $ target <#> map \t -> { head: t, tail: t }

  rope :: Array (Event (Maybe Segment)) <- makeRope head length grow incLength

  -- clear target on reaching it
  useEffect (delay 1 $ filter identity $ ((==) <<< Just) <$> head <*> target) $ const $ setTarget Nothing
  -- clear target when arrow key is pressed
  useEffect keyControl $ const $ setTarget Nothing

  D.div
    (klass_ "bg-slate-800 text-slate-100 p-8 h-screen grid gap-8 grid-rows-[auto_1fr] grid-cols-[auto_1fr]")
    [ descriptionPanel ~~ {}
    , D.div (klass_ "flex gap-6 justify-start items-stretch")
        [ controlPanel "length" "Length" $ D.div (klass_ "flex gap-4 items-center")
            [ textButton decLength "-1"
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
            , textButton incLength "+1"
            , D.span (klass_ "w-4") [text $ show <$> length]
            ]
        , controlPanel "grow" "Grow as I move" $ switch "grow" growState
        , controlPanel "motor" "Motor" $ switch "motor" motorState
        , puzzleInputPanel setInstructions
        ]
    , D.div
        Alt.do
          klass_ $ containerKlass <> " flex-1 relative cursor-pointer"
          click_ $ cb $ setTarget <=< mousePoint
        $ targetEl : (zipWith ropeSegment (cycleTo maxLength rainbow) rope)
    ]

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

ropeSegment :: String -> Event (Maybe Segment) -> Nut
ropeSegment klasses segment = Deku.do
  { transitionKlass, transitionEnd } <- transition (isJust <$> segment)
    { gone: "hidden"
    , here: "absolute"
    , enterFrom: "!w-0 !h-0"
    , enterTo: "w-6"
    , entered: ""
    , leaveFrom: "w-6"
    , leaveTo: "!w-0 !h-0"
    }

  D.div
    Alt.do
      D.OnTransitionend !:= transitionEnd
      klass
        $ pure (klasses <> " rounded-full border transition-all duration-200 left-1/2 top-1/2 ")
        <> transitionKlass

      style ado
        { head, tail } <- segment'
        turns <- turnsState
        let
          d = delta tail head
          width = (sqrt (d.x * d.x + d.y * d.y) + 1.0) * segmentWeightPx
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
      fold (\last cur -> closest last (floor last + cur) (Number.ceil last + cur)) 0.0
        $ turnsIn <$> segment'

    turnsIn :: Segment -> Number
    turnsIn { head, tail } = case delta tail head of
      { x: -1.0, y: 0.0 } -> 0.5
      d -> (d.x - 2.0) * (-0.125) * signum d.y

puzzleInputPanel :: (Array Point -> Effect Unit) -> Nut
puzzleInputPanel setInstructions = Deku.do
  (setOpen /\ open) <- useState false
  setInput /\ input <- useHot ""
  submit /\ submitted <- useState'
  setError /\ error <- useState (Nothing :: Maybe ParseError)

  useEffect (submitted *|> input) $ parseInput >>> case _ of
    Left err -> setError $ Just err
    Right instructions -> do
      setError Nothing
      setInstructions instructions
      setOpen false

  { transitionKlass, transitionEnd, transitionState } <- transition open
    { gone: "inset-0"
    , here: "right-0 top-0 shadow-xl"
    , enterFrom: closedWidth <> " h-24"
    , enterTo: "w-96 h-72"
    , entered: "w-96 h-72"
    , leaveFrom: "w-96 h-72"
    , leaveTo: closedWidth <> " h-24"
    }

  D.div (klass_ $ "relative space-y-2.5 " <> closedWidth)
    [ D.div
        Alt.do
          D.OnTransitionend !:= transitionEnd
          klass $ i containerKlass" flex flex-col gap-y-2.5 absolute z-10 transition-all duration-300 " <$> transitionKlass
        [ D.div (klass_ "flex justify-between items-start")
          [ controlLabel "puzzle-input" "Puzzle input"
          , DekuC.guard open $ iconButton (setOpen false) "×"
          ]
        , transitionState <#~> case _ of
            Gone -> textButton (setOpen true) "Add puzzle input"
            Here -> D.form
              Alt.do
                klass_ "h-full flex flex-col gap-2.5 items-end"
                D.OnSubmit !:= cb \e -> preventDefault e *> submit unit
              [ D.textarea
                  Alt.do
                    textInput_ setInput
                    klass_ "bg-slate-600 outline-none py-1.5 px-2.5 w-full h-full"
                  [text input]
              , D.div (klass_ "flex w-full justify-between items-start")
                  [ D.span (klass_ "text-red-300") [text $ parseErrorMessage <$> compact error]
                  , textButton mempty "Play"
                  ]
              ]
            _ -> mempty
        ]
    ]
  where
    closedWidth = "w-44"

parseInput :: String -> Either ParseError (Array Point)
parseInput input = runParser input $ P.fail "hi"

segmentWeightPx :: Number
segmentWeightPx = 24.0

segmentHalfWeightPx :: Number
segmentHalfWeightPx = segmentWeightPx / 2.0

controlPanel :: forall lock payload. String -> String -> Domable lock payload -> Domable lock payload
controlPanel name label contents =
  D.div (klass_ $ containerKlass <> " max-w-max space-y-2.5")
    [ controlLabel name label
    , contents
    ]

controlLabel :: String -> String -> Nut
controlLabel name label =
  D.label
    Alt.do
      D.For !:= name
      klass_ "font-bold italic text-slate-300 block"
    [text_ label]

switch :: String -> (Boolean -> Effect Unit) /\ Event Boolean -> Nut
switch name (setState /\ state) =
  D.input
    Alt.do
      D.Xtype !:= "checkbox"
      D.Name !:= name
      D.Checked <:=> show <$> state
      click $ setState <<< not <$> state
      klass_ "switch"
    []

textButton :: Effect Unit -> String -> Nut
textButton = styledButton "py-0.5 px-2 rounded border border-teal-400 text-teal-400 text-sm font-medium bg-teal-500/10 hover:bg-teal-500/25"

iconButton :: Effect Unit -> String -> Nut
iconButton = styledButton "text-2xl leading-none text-teal-400/75 hover:text-teal-400/100 cursor-pointer"

styledButton :: String -> Effect Unit -> String -> Nut
styledButton klass onClick label = D.button (klass_ klass <|> click_ onClick) [text_ label]

containerKlass :: String
containerKlass = "px-4 pt-2 pb-3 bg-slate-700 rounded-lg border-2 border-slate-600 min-w-max"

maybeFollow :: Maybe Point -> Maybe Point -> Maybe Point
maybeFollow tailM headM = do
  head <- headM
  Just $ fromMaybe head do
    tail <- tailM
    Just $ fromMaybe tail $ tail `follow` head

follow :: Point -> Point -> Maybe Point
follow follower target = do
  guard $ abs d.x > 1.0 || abs d.y > 1.0
  pure $ add follower $ hmap (trunc <<< signum) d
  where
     d = delta follower target

applyMotion :: Point -> Motion -> Point
applyMotion point = add point <<< case _ of
  Vector v -> v
  Target t -> hmap (trunc <<< signum) $ delta point t

add :: Point -> Point -> Point
add p1 p2 =
  { x: p1.x + p2.x
  , y: p1.y + p2.y
  }

delta :: Point -> Point -> Delta
delta from to = hmap toNumber
  { x: to.x - from.x
  , y: to.y - from.y
  }

vectorFromKey :: String -> Maybe Motion
vectorFromKey = map Vector <<< case _ of
  "ArrowUp" -> Just { x: 0, y: -1 }
  "ArrowDown" -> Just { x: 0, y: 1 }
  "ArrowLeft" -> Just { x: -1, y: 0 }
  "ArrowRight" -> Just { x: 1, y: 0 }
  _ -> Nothing

mousePoint :: WebEvent.Event -> Effect (Maybe Point)
mousePoint ev = do
  pixelRatio <- window >>= devicePixelRatio
  maybeRect <- traverse getBoundingClientRect $ El.fromEventTarget =<< currentTarget ev
  pure do
    mouseEvent <- Mouse.fromEvent ev
    rect <- maybeRect
    pure $ hmap (Int.ceil <<< (_ / (pixelRatio * segmentHalfWeightPx)))
      { x: toNumber (Mouse.offsetX mouseEvent) - (rect.width / 2.0)
      , y: toNumber (Mouse.offsetY mouseEvent) - (rect.height / 2.0)
      }

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
