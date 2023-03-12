module Main where

import Prelude

import Deku.Attributes (klass_)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Control (text_)
import Deku.Core (fixed, Nut)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import QualifiedDo.Alt as Alt

main :: Effect Unit
main = runInBody top

top :: Nut
top = fixed
  [ D.div
      Alt.do
        klass_ "m-8 p-4 bg-slate-700 text-slate-100 max-w-max"
    [text_ "Hellooo"]
  ]
