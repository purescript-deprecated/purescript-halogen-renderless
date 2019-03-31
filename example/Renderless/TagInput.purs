module Example.Renderless.TagInput where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (liftF)
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
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
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent as KE

type Action ps m v = Variant 
  ( handleInput :: String 
  , handleKey :: KE.KeyboardEvent
  , receive :: Input ps m v
  | v
  )

handleInput :: forall ps m v. String -> Action ps m v
handleInput = inj (SProxy :: _ "handleInput")

handleKey :: forall ps m v. KE.KeyboardEvent -> Action ps m v
handleKey = inj (SProxy :: _ "handleKey")

receive :: forall ps m v. Input ps m v -> Action ps m v
receive = inj (SProxy :: _ "receive")

data Query ps v a
  -- you can mount child components within the renderless component and send queries through to the
  -- child using SendQuery; its result will be passed back to the parent directly, bypassing the
  -- renderless component altogether. 
  -- 
  -- *** TODO: Experiment with triggering SendQuery in `handleAction` so that externally-provided
  -- actions can simply do this and handle the result without it having to leak out to the parent
  -- component at all.
  = SendQuery (ChildQueryBox ps (Maybe a)) 
  -- to preserve the existing semantics of actions as private, you can imperatively trigger your own
  -- provided actions if you wish, but cannot trigger internal component ones.
  | Perform (Variant v) a

type Slot ps out v
  = H.Slot (Query ps v) (Message out)

type StateStore ps m v =
  Store State (H.ComponentHTML (Action ps m v) ps m)

type State = 
  { tags :: Set String 
  , text :: String 
  }

newtype Input ps m v = Input
  { render :: State -> H.ComponentHTML (Action ps m v) ps m 
  }

derive instance newtypeInput :: Newtype (Input ps m v) _

type Message out = Variant
  ( tagAdded :: String
  | out 
  )

tagAdded :: forall out. String -> Message out
tagAdded = inj (SProxy :: _ "tagAdded")

attachInputProps 
  :: forall ps m v
   . State 
  -> Array (HH.IProp HTMLinput (Action ps m v)) 
  -> Array (HH.IProp HTMLinput (Action ps m v))
attachInputProps st = append 
  [ HP.value st.text
  , HE.onValueInput $ pure <<< handleInput
  , HE.onKeyDown $ pure <<< handleKey
  ]

component 
  :: ∀ ps out m v
   . MonadEffect m
  => (Variant v -> H.HalogenM (StateStore ps m v) (Action ps m v) ps (Message out) m Unit)
  -> H.Component HH.HTML (Query ps v) (Input ps m v) (Message out) m
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
  initialState :: Input ps m v -> StateStore ps m v
  initialState (Input { render }) = store render { tags: Set.fromFoldable [ "my-example-tag" ], text: "" }

  handleAction
    :: Action ps m v
    -> H.HalogenM (StateStore ps m v) (Action ps m v) ps (Message out) m Unit
  handleAction = flip onMatch handleExtraAction
    { handleInput: \str ->
        modifyState_ _ { text = str }

    , handleKey: \ev -> when (KE.code ev == "Enter") do
        H.liftEffect $ preventDefault (KE.toEvent ev)
        st <- getState
        when (st.text /= "") do
          modifyState_ _ { tags = Set.insert st.text st.tags, text = "" }
          H.raise $ tagAdded st.text
          handleAction $ handleInput "hello"

    , receive: \(Input { render }) -> 
        modifyStore_ render (\s -> s)
    }

  handleQuery
    :: forall a
     . Query ps v a
    -> H.HalogenM (StateStore ps m v) (Action ps m v) ps (Message out) m (Maybe a)
  handleQuery = case _ of
    SendQuery box -> do
      H.HalogenM $ liftF $ H.ChildQuery box
    
    Perform act a -> do
      handleAction $ unsafeCoerce act
      pure (Just a)
