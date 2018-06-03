module Component where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, runStore, seeks, store)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState)
import Data.Tuple (Tuple(..), snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query o a
  = Raise (o Unit) a
  | Receive (Input o) a

type StateStore o =
  Store State (H.ComponentHTML (Query o))

type State = Unit

type Input o = { render :: State -> H.ComponentHTML (Query o) }

data Message o
  = Emit (o Unit)

component :: ∀ eff m o
  . MonadAff eff m
 => H.Component HH.HTML (Query o) (Input o) (Message o) m
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where

  initialState :: Input o -> StateStore o
  initialState { render } = store render unit

  eval
    :: Query o
    ~> H.ComponentDSL (StateStore o) (Query o) (Message o) m
  eval = case _ of
    Raise query a -> do
      H.raise (Emit query)
      pure a

    Receive { render } a -> do
      let stateUpdate = \s -> s
      H.modify (updateStore render stateUpdate)
      pure a

getState :: ∀ m s a
   . MonadState (Store s a) m
  => m s
getState = map snd <<< pure <<< runStore =<< H.get

updateStore :: ∀ state html
  . (state -> html)
 -> (state -> state)
 -> Store state html
 -> Store state html
updateStore r f
  = (\(Tuple _ s) -> store r s) <<< runStore <<< seeks f
