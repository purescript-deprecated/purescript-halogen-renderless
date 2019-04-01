module Example.Page where

import Prelude

import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (match, onMatch)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Example.Component.Renderless.TagInput as TI
import Example.Component.Renderless.TagInputExtended as TIE
import Example.Component.Tag as Tag
import Halogen as H
import Halogen.HTML as HH

-- In this module, we'll create a page that displays two tag inputs. The first will 
-- use the basic renderless component, just simply using its flexibility in rendering
-- to provide a custom renderer. The next will use the fancy extended renderless component.

-----
-- Page

-- Our page will handle both kinds of tag input. The first kind is not extended, so we'll only
-- need to handle messages from the renderless component. The second kind is extended, so we'll
-- need to additionally handle the extra provided messages. Luckily, we can share our handling
-- code between the two because of the shared variants.
data Action 
  = HandleTagInput (TI.Message ())
  | HandleTagInputExtended (TI.Message TIE.ExtraMessages)

-- There are no extended child components, messages, or actions for the basic tag input, but
-- we'll need to supply those types for the extended version.
type ChildSlots =
  ( tagInput :: TI.Slot () () () Unit 
  , tagInputExtended :: TI.Slot TIE.TagInputChildSlots TIE.ExtraMessages TIE.ExtraActions Unit
  )

-- These identifiers will make it a little nicer to refer to the type-level strings that label each
-- type of component.
_tagInput = SProxy :: SProxy "tagInput"
_tagInputExtended = SProxy :: SProxy "tagInputExtended"

-- Finally, let's turn to our page component. It will simply 
component :: ∀ m. MonadEffect m => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    }
  where
  render :: Unit -> H.ComponentHTML Action ChildSlots m
  render _ = 
    HH.div_ 
      [ HH.h2_
          [ HH.text "Basic Tag Input" ]
      , HH.p_
          [ HH.text "The basic tag input supports adding tags, but not removal, and tags are simple strings." ]
      , HH.slot _tagInput unit (TI.component \_ -> pure unit) (TI.Input { render: renderTagInput }) (Just <<< HandleTagInput)
      , HH.h2_
          [ HH.text "Extended Tag Input" ]
      , HH.p_
          [ HH.text "The extended tag input also supports removal, outputs removal messages, and manages tags as further child components" ]
      , HH.slot _tagInputExtended unit (TI.component TIE.handleExtraActions) (TI.Input { render: renderTagInputExtended }) (Just <<< HandleTagInputExtended) 
      ]

  -- We'll need to handle the `tagAdded` message for both the basic and extended components. Fortunately,
  -- because they're variants, we can write a handler once and share it between both the basic and the
  -- extended components. The extended component will need to handle the additional removal message.
  handleTagInputMessage :: TI.Message () -> H.HalogenM Unit _ _ _ m Unit
  handleTagInputMessage = match
    { tagAdded: \index -> do
        -- The `send` function lets us send a query through the renderless component to
        -- the inner `Tag` component and receive the result directly. The original component tells
        -- us the newly-added index, but we want to know the text of the new tag. Can't fork the
        -- original project? Let's use `send` to retrieve the tag name by index.
        -- the index of the newly-created tag, essentially bypassing the renderless component
        -- to do so.
        mbTag <- TI.send _tagInputExtended unit TIE._tag index (Tag.GetInitialTag identity)
        case mbTag of
          Nothing -> Console.log "I don't know where that tag was added."
          Just tag -> Console.log $ "Added tag " <> tag <> " at index: " <> show index
    }
  
  -- The only actions the page component manages are for handling messages from the basic and 
  -- extended tag input components. Since the messages are half-shared, we'll re-use the same 
  -- handler for both and simply extend with more handling for the tag input.
  handleAction :: _ -> H.HalogenM Unit _ _ _ m Unit
  handleAction = case _ of
    HandleTagInput msg -> 
      handleTagInputMessage msg
    HandleTagInputExtended msg -> onMatch
      { tagRemoved: \str -> do 
          Console.log $ "Removed " <> str
      } handleTagInputMessage msg


-----
-- SIMPLE RENDERING

-- The key promise of a renderless component is that you can handle rendering however you see fit.
-- Let's go ahead and provide our own render function for the basic tag input. We'll throw in a
-- twist: tags will render above the input, not below.

-- Remember that you can use any action or state from the renderless component to write your render
-- function. You can also use any values from the *parent* component state, as they will update
-- in the child when the parent re-renders (that's how you can extend the renderless component state
-- from the parent). You can also extend this with more actions as we've already done or proceed
-- to include further child components.

renderTagInput :: forall m. TI.State -> H.ComponentHTML (TI.Action () m ()) () m
renderTagInput st = 
  HH.div_ 
    [ HH.ul_ $ map (\tag -> HH.li_ [ HH.text tag ]) st.tags
    -- We can use the `attachInputProps` helper to attach all the relevant actions necessary to
    -- the input field rather than write them all out manually.
    , HH.input $ TI.attachInputProps st [] 
    ]

-----
-- EXTENDED RENDERING

-- Let's put all our extensions to use. We'll render out the tags again, but this time, we'll 
-- render them as individual tag components with lots of new behavior (you can now edit and delete
-- tags, not just create).

renderTagInputExtended
  :: forall m
   . MonadEffect m
  => TI.State 
  -> H.ComponentHTML (TI.Action TIE.TagInputChildSlots m TIE.ExtraActions) TIE.TagInputChildSlots m
renderTagInputExtended st = 
  HH.div_ 
    [ HH.input $ TI.attachInputProps st [] 
    -- Rather than list out all the tags as strings, we'll instead use them to initialize individual
    -- `Tag` components, which we'll then control.
    , HH.ul_ $ st.tags # mapWithIndex \i x -> 
        HH.li_
          [ HH.slot TIE._tag i Tag.component { initialTag: x, index: i } (Just <<< TIE.handleTag) ]
    ]