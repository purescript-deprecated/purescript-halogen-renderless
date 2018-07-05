module Example.Child where

import Prelude

import Renderless.State (modifyStore_)
import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- | All renderless components need at least two queries:
-- |
-- | - `Raise`: A wrapper for another component's query. This is what
-- |            enables users of this renderless component to extend
-- |            its functionality with new behaviors from their query
-- |            algebra.
-- | - `Receive`: The standard query for components that need to
-- |              update when new `Input` arrives from the parent.
-- |              This is what enables users of this component to
-- |              embed new data via the render function they pass
-- |              as input.
data Query o a
  = Raise (o Unit) a
  | Receive (Input o) a

-- | All renderless components will use the `Store` comonad to hold
-- | on to the render function. The component can receive its render
-- | function in just one of two ways: first, as an argument, and
-- | second, via its `Input`. If the render function is passed as
-- | an argument, you can no longer continue to update the data you
-- | embedded via the parent (this is unacceptable). If the render
-- | function is passed via `Input`, then we have no way to provide
-- | it to the component configuration!
-- |
-- | What do we do?
-- |
-- | The answer is to make the overall component context not `State`
-- | but instead `Store` containing the render function and state,
-- | and then to use `extract` from `Control.Comonad.Store` to
-- | recover the render function.
-- |
-- | If this is all a bit much, just write your state as usual,
-- | then use it in this `Store` type, and make this your component
-- | `State` and things will work out.
type StateStore o =
  Store State (H.ComponentHTML (Query o))

-- | You can largely ignore `Store` and just write and manage your
-- | component state as you usually would.
type State = Unit

-- | Your input is going to have to take at least the render function
-- | for the component, but it can take whatever else you want, too.
type Input o = { render :: State -> H.ComponentHTML (Query o) }

-- | All renderless components must have at least one message:
-- |
-- | - `Emit`: This message wraps another component's query. Used in
-- |           conjunction with `Raise`, this is how the other
-- |           component can be notified when one of its queries was
-- |           triggered inside the renderless component and can
-- |           then evaluate it.
data Message o
  = Emit (o Unit)

-- | You can largely define your component as usual, but you'll
-- | need to ensure your state functions operate within `Store`.
-- |
-- | Fortunately, end users of your component will not have to care
-- | about `Store` at all and can use your component as they would
-- | any other.
component :: ∀ m o. MonadAff m => H.Component HH.HTML (Query o) (Input o) (Message o) m
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where

  -- | When you create the initial state for the component, you will
  -- | need to use the `store` function from `Control.Comonad.Store`
  -- | and give it the render function as its first argument. `store`
  -- | will take the state as its second argument, so you can write
  -- | your `initialState` the same as usual and just remember to
  -- | stick `store render $ ` in front of it.
  initialState :: Input o -> StateStore o
  initialState { render } = store render unit

  -- | Every renderless function will need at least two handlers, one
  -- | for each of the necessary queries. Since you are not working
  -- | within `State` anymore, but `StateStore`, you can't use
  -- | `H.get` and `H.modify` as usual. You have to first reach into
  -- | `Store` to where your state has been stored. However, you can
  -- | use the `getState` and `seeks` functions to mimic how you
  -- | would usually write eval functions.
  -- |
  -- | End users of your component do not have to worry about this
  -- | and can interact with your component as usual.
  eval
    :: Query o
    ~> H.ComponentDSL (StateStore o) (Query o) (Message o) m
  eval = case _ of
    -- | When the `Raise` query is triggered, we now have access
    -- | to a query embedded from another component. We can't do
    -- | anything with it, but we can notify that component with
    -- | the query so they can evaluate it. This enables the other
    -- | component to embed behavior into this one.
    Raise query a -> do
      H.raise (Emit query)
      pure a

    -- | When the `Receive` query is triggered, it means we have
    -- | new input, which will at least include an updated render
    -- | function. You can decide how you'd like to update your
    -- | state given the new input as always; if you don't want to
    -- | update it, just use `id` here. However, you ALWAYS need
    -- | to update your `StateStore` with the new render function
    -- | or else you will lose the ability to embed data into your
    -- | render function from another component and therefore the
    -- | entire reason to use `Store` in the first place.
    Receive { render } a -> do
      modifyStore_ render (\s -> s)
      pure a

