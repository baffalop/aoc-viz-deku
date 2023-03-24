module Main
  ( main
  , follow
  )
  where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Apply (lift2)
import Data.Array as Array
import Data.Compactable (compact)
import Data.Filterable (filter)
import Data.Int (toNumber, trunc)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Number (abs, sqrt, floor, ceil)
import Data.Ord (signum)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (!:=), (<:=>))
import Deku.Attributes (klass, klass_, style)
import Deku.Control (text)
import Deku.Core (Domable, Nut, fixed)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useMemoized, useState)
import Deku.Listeners (slider_)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (Event, fold, withLast)
import FRP.Event.Keyboard as Key
import QualifiedDo.Alt as Alt
import Web.Event.Event (stopPropagation)

type Hook a = forall lock payload. (a -> Domable lock payload) -> Domable lock payload

type Point = { x :: Int, y :: Int }
type Vec = { x :: Number, y :: Number }
type Segment = { head :: Point, tail :: Point }

origin :: Point
origin = { x: 0, y: 0 }

initLength :: Int
initLength = 10

maxLength :: Int
maxLength = 30

main :: Effect Unit
main = runInBody Deku.do
  setLength /\ length <- useState initLength

  let
    head :: Event Point
    head = pure origin <|> fold add origin (compact $ vectorFromKey <$> Key.down)

  rope :: Array (Event (Maybe Segment)) <- makeRope length head

  fixed
    [ D.div
        (klass_ "bg-slate-800 p-8 flex flex-col gap-8 text-slate-100 h-screen")
        [ D.div (klass_ $ containerKlass <> " space-x-4 max-w-max")
            [ D.input
                Alt.do
                  slider_ $ setLength <<< trunc
                  D.OnKeydown !:= cb stopPropagation
                  klass_ "cursor-pointer"
                  D.Value <:=> show <$> length
                  D.Step !:= "1"
                  D.Min !:= "1"
                  D.Max !:= show maxLength
              []
            , D.span_ [text $ show <$> length]
            ]
        , D.div (klass_ $ containerKlass <> " flex-1 relative")
            $ rope <#> ropeSegment "border-red-400 bg-red-500/40"
        ]
    ]
  where
    containerKlass = "p-4 bg-slate-700"

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

makeRope :: Event Int -> Event Point -> Hook (Array (Event (Maybe Segment)))
makeRope length initialHead f = unfold 1 (Just <$> initialHead) []
  where
    unfold i head rope
      | i > maxLength = f rope
      | otherwise = Deku.do
        tail <- useMemoized $ dedup
          $ fold maybeFollow Nothing
          $ maybeIf <<< (i <= _) <$> length <*> compact head
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
dedup =
  withLast
  >>> filter (\{ last, now } -> Just now /= last)
  >>> (_ <#> _.now)
