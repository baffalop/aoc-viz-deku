module Utils.Deku
  ( Hook
  , transition
  , useToggle
  )
  where

import Prelude

import Data.Interpolate (i)
import Data.Tuple.Nested ((/\), type (/\))
import Deku.Core (Domable)
import Deku.Do as Deku
import Deku.Hooks (useEffect, useState, useState')
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class ((*|>), (<*|>))

type Hook a = forall lock payload. (a -> Domable lock payload) -> Domable lock payload

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

useToggle :: Boolean -> Hook (Effect Unit /\ (Boolean -> Effect Unit) /\ Event Boolean)
useToggle initial f = Deku.do
  toggle /\ toggled <- useState'
  setState /\ state <- useState initial
  useEffect (toggled *|> state) $ setState <<< not
  f $ toggle unit /\ setState /\ state
