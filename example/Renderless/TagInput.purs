module Example.Renderless.TagInput where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (liftF)
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Class (class MonadEffect)
import Halogen (defaultEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.ChildQuery (ChildQueryBox)
import Renderless.State (getState, modifyState_, modifyStore_)
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent as KE

data Action pq ps m
  = HandleInput String
  | HandleKey KE.KeyboardEvent
  | RemoveTag String
  | Receive (Input pq ps m)
  | Raise (pq Unit)

data Query ps a
  = Send (ChildQueryBox ps (Maybe a)) 

type Slot pq ps
  = H.Slot (Query ps) (Message pq)

type StateStore pq ps m =
  Store State (H.ComponentHTML (Action pq ps m) ps m)

type State = 
  { tags :: Set String 
  , text :: String 
  }

type Input pq ps m =
  { render :: State -> H.ComponentHTML (Action pq ps m) ps m 
  }

data Message pq
  = Emit (pq Unit)

attachInputProps 
  :: forall pq ps m
   . State 
  -> Array (HH.IProp HTMLinput (Action pq ps m)) 
  -> Array (HH.IProp HTMLinput (Action pq ps m))
attachInputProps st = append 
  [ HP.value st.text
  , HE.onValueInput $ pure <<< HandleInput
  , HE.onKeyDown $ pure <<< HandleKey
  ]

component 
  :: ∀ pq ps m
   . MonadEffect m
  => H.Component HH.HTML (Query ps) (Input pq ps m) (Message pq) m
component =
  H.mkComponent
    { initialState
    , render: extract
    , eval: H.mkEval $ defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Input pq ps m -> StateStore pq ps m
  initialState { render } = store render { tags: Set.fromFoldable [ "my-example-tag" ], text: "" }

  handleAction
    :: Action pq ps m
    -> H.HalogenM (StateStore pq ps m) (Action pq ps m) ps (Message pq) m Unit
  handleAction = case _ of
    HandleInput str ->
      modifyState_ _ { text = str }

    HandleKey ev -> case KE.code ev of
      "Enter" -> do 
        H.liftEffect $ preventDefault (KE.toEvent ev)
        st <- getState
        when (st.text /= "") do
          modifyState_ _ { tags = Set.insert st.text st.tags, text = "" }
      _ -> 
        pure unit
    
    RemoveTag tag ->
      modifyState_ \st -> st { tags = Set.delete tag st.tags }

    Raise act -> 
      H.raise (Emit act)

    Receive { render } -> 
      modifyStore_ render (\s -> s)

  handleQuery
    :: forall a
     . Query ps a
    -> H.HalogenM (StateStore pq ps m) (Action pq ps m) ps (Message pq) m (Maybe a)
  handleQuery = case _ of
    Send box -> 
      H.HalogenM $ liftF $ H.ChildQuery box