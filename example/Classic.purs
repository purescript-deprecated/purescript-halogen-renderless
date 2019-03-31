-- | A classic tag input
module Example.Classic where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Example.Renderless.TagInput as TagInput
import Halogen (defaultEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Renderless.State (modifyState_)

-----
-- Extensions

-- You can extend a renderless component in two significant ways:
--   - you can add new actions to the component, if you provide a corresponding handler
--   - you can add your own output messages and handle them by pattern-matching on `Emit`

-- The handler you provide can do anything that was previously possible in the handleActions
-- function and simply adds to the existing handler.

-- We're going to extend this component to be able to remove tags, as well. We'd like to have
-- a message when a tag has been removed, to add to the message when a tag has been added
type ExtraMessages = 
  ( tagRemoved :: String )

tagRemoved :: String -> TagInput.Message ExtraMessages
tagRemoved = inj (SProxy :: _ "tagRemoved") 

-- We're also going to add an action that will be triggered when a tag is clicked
type ExtraActions = 
  ( removeTag :: String )

removeTag :: forall v. String -> Variant (removeTag :: String | v)
removeTag = inj (SProxy :: _ "removeTag") 

-- Finally, we'll handle that action by removing the tag from TagInput state and raising our
-- new message.
type HandleExtraActions act out m =
  H.HalogenM (TagInput.StateStore () m act) (TagInput.Action () m act) () (TagInput.Message out) m Unit

handleExtraActions 
  :: forall m
   . Variant ExtraActions 
  -> HandleExtraActions ExtraActions ExtraMessages m
handleExtraActions = match
  { removeTag: \tag -> do
      modifyState_ \st -> st { tags = Set.delete tag st.tags }
      H.raise (tagRemoved tag)
  }

-- and now we'll write the render function for the extended component
renderTagInput
  :: forall m
   . TagInput.State 
  -> H.ComponentHTML (TagInput.Action () m ExtraActions) () m
renderTagInput st@{ tags } = 
  HH.div_ 
    [ HH.input $ TagInput.attachInputProps st [] 
    , HH.div_ $ Set.toUnfoldable tags # map \x -> 
        HH.li 
          [ HE.onClick \_ -> Just (removeTag x) ] 
          [ HH.text x ]
    ]

-----
-- Component

-- Now we can make our new, extended tag input 

data Action 
  -- We will handle all messages already output by the component, plus our extra messages
  = HandleTagInput (TagInput.Message ExtraMessages)

type ChildSlots =
  ( tagInput :: TagInput.Slot () ExtraMessages ExtraActions Unit )

component :: ∀ m. MonadEffect m => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ defaultEval
        { handleAction = handleAction }
    }
  where
  render :: Unit -> H.ComponentHTML Action ChildSlots m
  render _ = 
    HH.div_ 
      [ HH.slot (SProxy :: _ "tagInput") unit (TagInput.component handleExtraActions) (TagInput.Input { render: renderTagInput }) (Just <<< HandleTagInput) 
      ]
  
  -- we can write our handler pretty much as usual, but because Message is a variant, we'll use `match` 
  -- instead of `case`. Notice how you can only use `Perform` to trigger your own provided actions, not
  -- just any action at all.
  handleAction = case _ of
    HandleTagInput msg -> msg # match
      { tagRemoved: \str -> do 
          log $ "Removed " <> str
      , tagAdded: \str -> do
          log $ "Added " <> str
          _ <- H.query (SProxy :: _ "tagInput") unit $ H.tell $ TagInput.Perform (removeTag str)
          pure unit
      }
      