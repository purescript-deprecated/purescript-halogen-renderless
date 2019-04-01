module Example.Component.Renderless.TagInput where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (liftF)
import DOM.HTML.Indexed (HTMLinput)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
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
import Renderless.State (getState, modifyState, modifyState_, modifyStore_)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent as KE

-----
-- ACTIONS

-- This basic tag input will handle text input in a text field, key presses like "Enter", and
-- will update with new input when its parent re-renders. These are considered internal logic
-- for the component, in contrast with the Query type below, which is the component's public
-- interface.
type Action ps m v = Variant 
  ( handleInput :: String 
  , handleKey :: KE.KeyboardEvent
  , receive :: Input ps m v
  | v
  )

-- For convenience, we'll make it possible to use each of these actions just as you ordinarily would
-- use a data constructor. Where you would previously do \_ -> HandleInput you can now do
-- \_ -> handleInput. But the upside is that this is now extensible.

handleInput :: forall ps m v. String -> Action ps m v
handleInput = inj (SProxy :: _ "handleInput")

handleKey :: forall ps m v. KE.KeyboardEvent -> Action ps m v
handleKey = inj (SProxy :: _ "handleKey")

receive :: forall ps m v. Input ps m v -> Action ps m v
receive = inj (SProxy :: _ "receive")

-- For convenience, we'll provide a helper function which can be used on an input field's props in a 
-- render function to attach several actions at once. End users can essentially ignore how to wire
-- everything up and just use this function on the text input the user will interact with.

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


-----
-- QUERY

-- This query type is a public interface to the component, which can be triggered by a parent
-- component at any time. We'll support imperatively selecting a tag, but we'll also support
-- two behaviors that make this useful as a renderless component: sending queries through and
-- performing an action provided by the parent component.
--
-- Notably, the public interface is not extensible. If you need to extend the public interface
-- with some new behavior you'd like to trigger from the parent, try providing it as an action and
-- then using `Perform`. If it also needs to return information, try providing an action that raises
-- a custom message and extending the `Message` type as well.

data Query ps v a
  = SelectTag String a
  -- A renderless component can be provided with further child components, mounted via the render
  -- function provided to the component. When those child components have public interfaces that 
  -- you would like to be accessible from the renderless component's parent, then you can allow
  -- direct access to the child query algebra from the parent while 'skipping' the renderless 
  -- component altogether. It's as if there were no renderless component. 
  | SendQuery (ChildQueryBox ps (Maybe a)) 
  -- The `Action` type of a renderless component is kept private. Sometimes, though, it's convenient 
  -- to extend a renderless component with new actions, some of which you'd like to be able to 
  -- imperatively trigger from a parent component. The `Perform` query allows you to do this by
  -- providing access to any actions you extended the renderless component with. It does not allow
  -- access to any private actions for the renderless component.
  | Perform (Variant v) a


-----
-- MESSAGE

-- Messages are the outputs of the component, which parent components can subscribe to and handle.
-- We'll use Variant once again to handle this.

-- This component will emit messages when a tag has been added to the list. We'll do something silly:
-- we'll notify end users about the index of the tag, not its contents. This gives us an excuse to 
-- work around the bad design later and show off a little more Renderless.
type Message out = Variant
  ( tagAdded :: Int
  | out 
  )

tagAdded :: forall out. Int -> Message out
tagAdded = inj (SProxy :: _ "tagAdded")


-----
-- SLOT

-- This convenience type synonym makes it a little nicer to provide this slot in a row to the
-- parent component.
type Slot ps out v
  = H.Slot (Query ps v) (Message out)


-----
-- COMPONENT TYPES

-- Renderless components use the Store comonad to hold on to their state and render functions. This
-- is critical because it allows the render function to be passed via the `Input` type and to be 
-- refreshed with the `receive` lifecycle method for the component. If you just pass the render 
-- function as an argument, then it will retain whatever values it had from the parent component at
-- the time the component was mounted. When the parent's state values change, the child will not
-- update accordingly. Passing via `Input` fixes this problem. TODO: This needs a _way_ better
-- explanation.
type StateStore ps m v =
  Store State (H.ComponentHTML (Action ps m v) ps m)

-- This component will maintain an array of tags and the current text in the text input.
type State = 
  { tags :: Array String 
  , text :: String 
  }

-- The render function must be provided via `input` if it is to contain any information from the
-- parent state; otherwise, the render function will remain with its initial values from the parent
-- for the duration of the component's life. This requires a newtype to accommodate the cyclical
-- reference to the Action variant type.
newtype Input ps m v = Input
  { render :: State -> H.ComponentHTML (Action ps m v) ps m 
  }

derive instance newtypeInput :: Newtype (Input ps m v) _

-- The component is, in most respects, a usual one. The main two differences: the render function 
-- is the `extract` function from the Store comonad, and it accepts a function as an argument that
-- instructs the component how to handle any new actions provided by a parent component.
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
  initialState (Input { render }) = store render { tags: mempty, text: "" }

-- The `handleAction` function will handle any actions arising from the renderless component, and
-- will defer to the provided `handleExtraActions` function for any extended functionality from
-- the parent. It's best to leave this in public scope so that it can be referenced recursively
-- from the `handleExtraActions` function when that function is being written in another file.
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
      when (st.text /= "" && not (st.text `Array.elem` st.tags)) do
        newState <- modifyState _ { tags = Array.snoc st.tags st.text, text = "" }
        H.raise $ tagAdded (Array.length newState.tags - 1)

        -- you can recursively call here
        -- handleAction handleExtraActions $ handleInput "hello-auto-gen"

  , receive: \(Input { render }) -> 
      modifyStore_ render (\s -> s)
  } 

-- The `handleQuery` function will handle any received queries, including the `Perform` query, which
-- allows any extensible action to be called from the parent imperatively. This function should also
-- remain in the global scope so that it can be used when writing the `handleExtraActions` function.
handleQuery
  :: forall ps out m v a
   . MonadEffect m
  -- a function to handle additional actions provided by the parent
  => (Variant v -> H.HalogenM (StateStore ps m v) (Action ps m v) ps (Message out) m Unit)
  -> Query ps v a
  -> H.HalogenM (StateStore ps m v) (Action ps m v) ps (Message out) m (Maybe a)
handleQuery handleExtraActions = case _ of
  SelectTag str a -> do
    st <- getState
    when (str /= "" && not (str `Array.elem` st.tags)) do
      modifyState_ _ { tags = Array.snoc st.tags str, text = "" }
    pure (Just a)

  SendQuery box -> do
    H.HalogenM $ liftF $ H.ChildQuery box
  
  Perform act a -> do
    handleAction handleExtraActions $ unsafeCoerce act
    pure (Just a)

-----
-- SEND

-- The most intimidating function of all is `send`, which allows you to conveniently send a query
-- through the renderless component and receive the result. A parent component can thus essentially
-- ignore the renderless component standing between it and the component it wants to interact with.
--
-- ```purescript
-- -- in parent component...
-- handleAction = case _ of
--   MyAction -> do
--     --             (renderless) (inner component) (inner component query)
--     result <- send _tagInput 10 _innerComponent 0 MyQuery
--     doSomething result
-- ````
send
  :: ∀ outerSym outerSlot innerSym innerSlot cq cm ps v a pps t0 t1 t2 t3 t4 t5 t6
   . Row.Cons outerSym (Slot.Slot (Query ps v) t1 outerSlot) t2 pps
  => Row.Cons innerSym (Slot.Slot cq cm innerSlot) t0 ps
  => IsSymbol outerSym
  => IsSymbol innerSym
  => Ord outerSlot
  => Ord innerSlot
  => SProxy outerSym
  -> outerSlot 
  -> SProxy innerSym 
  -> innerSlot
  -> cq a
  -> H.HalogenM t3 t4 pps t5 t6 (Maybe a)
send outerLabel outerSlot innerLabel innerSlot query = 
  H.query outerLabel outerSlot 
    $ SendQuery  
    $ mkChildQueryBox 
    $ ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup innerLabel innerSlot) query identity
