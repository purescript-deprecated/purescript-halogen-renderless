module Example.Renderless.TagInput where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (liftF)
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Variant (SProxy(..), Variant, inj, onMatch)
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

newtype Action pq ps m v = Action (Variant
  ( handleInput :: String 
  , handleKey :: KE.KeyboardEvent
  , receive :: Input pq ps m v
  , raise :: pq Unit
  | v
  ))

derive instance newtypeAction :: Newtype (Action pq ps m v) _

handleInput :: forall pq ps m v. String -> Action pq ps m v
handleInput = Action <<< inj (SProxy :: _ "handleInput")

handleKey :: forall pq ps m v. KE.KeyboardEvent -> Action pq ps m v
handleKey = Action <<< inj (SProxy :: _ "handleKey")

receive :: forall pq ps m v. Input pq ps m v -> Action pq ps m v
receive = Action <<< inj (SProxy :: _ "receive")
  
raise :: forall pq ps m v. pq Unit -> Action pq ps m v
raise = Action <<< inj (SProxy :: _ "raise")

data Query ps a
  = Send (ChildQueryBox ps (Maybe a)) 

type Slot pq ps
  = H.Slot (Query ps) (Message pq)

slotLabel :: SProxy "tagInput"
slotLabel = SProxy

type StateStore pq ps m v =
  Store State (H.ComponentHTML (Action pq ps m v) ps m)

type State = 
  { tags :: Set String 
  , text :: String 
  }

type Input pq ps m v =
  { render :: State -> H.ComponentHTML (Action pq ps m v) ps m 
  }

data Message pq
  = Emit (pq Unit)

attachInputProps 
  :: forall pq ps m v
   . State 
  -> Array (HH.IProp HTMLinput (Action pq ps m v)) 
  -> Array (HH.IProp HTMLinput (Action pq ps m v))
attachInputProps st = append 
  [ HP.value st.text
  , HE.onValueInput $ pure <<< handleInput
  , HE.onKeyDown $ pure <<< handleKey
  ]

component 
  :: ∀ pq ps m v
   . MonadEffect m
  => (Variant v -> H.HalogenM (StateStore pq ps m v) (Action pq ps m v) ps (Message pq) m Unit)
  -> H.Component HH.HTML (Query ps) (Input pq ps m v) (Message pq) m
component handleExtraAction =
  H.mkComponent
    { initialState
    , render: extract
    , eval: H.mkEval $ defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< receive
        }
    }
  where
  initialState :: Input pq ps m v -> StateStore pq ps m v
  initialState { render } = store render { tags: Set.fromFoldable [ "my-example-tag" ], text: "" }

  handleAction
    :: Action pq ps m v
    -> H.HalogenM (StateStore pq ps m v) (Action pq ps m v) ps (Message pq) m Unit
  handleAction = unwrap >>> flip onMatch handleExtraAction
    { handleInput: \str ->
        modifyState_ _ { text = str }

    , handleKey: \ev -> when (KE.code ev == "Enter") do
        H.liftEffect $ preventDefault (KE.toEvent ev)
        st <- getState
        when (st.text /= "") do
          modifyState_ _ { tags = Set.insert st.text st.tags, text = "" }
          handleAction $ handleInput "hello"

    , raise: \act -> 
        H.raise (Emit act)

    , receive: \{ render } -> 
        modifyStore_ render (\s -> s)
    }

  handleQuery
    :: forall a
     . Query ps a
    -> H.HalogenM (StateStore pq ps m v) (Action pq ps m v) ps (Message pq) m (Maybe a)
  handleQuery = case _ of
    Send box -> 
      H.HalogenM $ liftF $ H.ChildQuery box