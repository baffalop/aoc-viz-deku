module Utils.FRP where

import Prelude

import Data.Filterable (filter)
import Data.Maybe (Maybe(..))
import Deku.Core (Domable)
import FRP.Event (Event, withLast)
import FRP.Event.Class ((<|*))

type Hook a = forall lock payload. (a -> Domable lock payload) -> Domable lock payload

dedup :: forall a. Eq a => Event a -> Event a
dedup event =
  event <|* filter (\{ last, now } -> Just now /= last) (withLast event)
