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

data Query a
  = Receive Input a

type StateStore =
  Store State (H.ComponentHTML Query)

type State = Unit

type Input = { render :: State -> H.ComponentHTML Query }

type Message = Void

component :: ∀ eff m
  . MonadAff eff m
 => H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where

  initialState :: Input -> StateStore
  initialState { render } = store render unit

  eval :: Query ~> H.ComponentDSL StateStore Query Message m
  eval = case _ of
    Receive { render } a -> do
      st <- H.get
      H.put $ updateStore render (\s -> s) st
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
