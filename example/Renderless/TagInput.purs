module Example.Renderless.TagInput where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (liftF)
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, inj, onMatch)
import Effect.Class (class MonadEffect)
import Halogen (defaultEval)
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.ChildQuery (ChildQuery(..), ChildQueryBox, mkChildQueryBox)
import Prim.Row as Row
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
  = SelectTag String a
  -- you can mount child components within the renderless component and send queries through to the
  -- child using SendQuery; its result will be passed back to the parent directly, bypassing the
  -- renderless component altogether. 
  -- 
  -- *** TODO: Experiment with triggering SendQuery in `handleAction` so that externally-provided
  -- actions can simply do this and handle the result without it having to leak out to the parent
  -- component at all.
  | SendQuery (ChildQueryBox ps (Maybe a)) 
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

-- The render function must be provided via `input` if it is to contain any information from the
-- parent state; otherwise, the render function will remain with its initial values from the parent
-- for the duration of the component's life. 
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
component handleExtraActions =
  H.mkComponent
    { initialState
    , render: extract
    , eval: H.mkEval $ defaultEval
        { handleAction = handleAction handleExtraActions
        , handleQuery = handleQuery handleExtraActions
        , receive = Just <<< receive
        }
    }
  where
  initialState :: Input ps m v -> StateStore ps m v
  initialState (Input { render }) = store render { tags: Set.fromFoldable [ "my-example-tag" ], text: "" }

handleAction
  :: forall ps out m v
   . MonadEffect m
  -- a function to handle additional actions provided by the parent
  => (Variant v -> H.HalogenM (StateStore ps m v) (Action ps m v) ps (Message out) m Unit)
  -> Action ps m v
  -> H.HalogenM (StateStore ps m v) (Action ps m v) ps (Message out) m Unit
handleAction handleExtraActions = flip onMatch handleExtraActions
  { handleInput: \str ->
      modifyState_ _ { text = str }

  , handleKey: \ev -> when (KE.code ev == "Enter") do
      H.liftEffect $ preventDefault (KE.toEvent ev)
      st <- getState
      when (st.text /= "") do
        modifyState_ _ { tags = Set.insert st.text st.tags, text = "" }
        H.raise $ tagAdded st.text
        handleAction handleExtraActions $ handleInput "hello-auto-gen"

  , receive: \(Input { render }) -> 
      modifyStore_ render (\s -> s)
  } 

-- must be in global scope to make this handler accessible for extension
handleQuery
  :: forall ps out m v a
   . MonadEffect m
  -- a function to handle additional actions provided by the parent
  => (Variant v -> H.HalogenM (StateStore ps m v) (Action ps m v) ps (Message out) m Unit)
  -> Query ps v a
  -> H.HalogenM (StateStore ps m v) (Action ps m v) ps (Message out) m (Maybe a)
handleQuery handleExtraActions = case _ of
  SelectTag str a -> do
    modifyState_ \st -> st { tags = Set.insert st.text st.tags, text = "" }
    pure (Just a)

  SendQuery box -> do
    H.HalogenM $ liftF $ H.ChildQuery box
  
  Perform act a -> do
    handleAction handleExtraActions $ unsafeCoerce act
    pure (Just a)

send
  :: ∀ outerSym outerSlot sym slot cq cm ps v a pps t0 t1 t2 t3 t4 t5 t6
   . Row.Cons sym (Slot.Slot cq cm slot) t0 ps
  => Row.Cons outerSym (Slot.Slot (Query ps v) t1 outerSlot) t2 pps
  => IsSymbol outerSym
  => IsSymbol sym
  => Ord outerSlot
  => Ord slot
  => SProxy outerSym
  -> outerSlot 
  -> SProxy sym 
  -> slot
  -> cq a
  -> H.HalogenM t3 t4 pps t5 t6 (Maybe a)
send outerLabel outerSlot innerLabel innerSlot query = 
  H.query outerLabel outerSlot 
    $ SendQuery  
    $ mkChildQueryBox 
    $ ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup innerLabel innerSlot) query identity
