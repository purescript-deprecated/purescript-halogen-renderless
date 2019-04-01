-- | A classic tag input
module Example.Classic where

import Prelude

import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
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

-- we're also going to mount further components inside the render function for the tag input
type InnerChildSlots = ( annotation :: H.Slot InnerQuery Void Int )

-- Finally, we'll handle that action by removing the tag from TagInput state and raising our
-- new message.
type HandleExtraActions act out m =
  H.HalogenM (TagInput.StateStore InnerChildSlots m act) (TagInput.Action InnerChildSlots m act) InnerChildSlots (TagInput.Message out) m Unit

handleExtraActions 
  :: forall m
   . MonadEffect m
  => Variant ExtraActions 
  -> HandleExtraActions ExtraActions ExtraMessages m
handleExtraActions = match
  { removeTag: \tag -> do
      modifyState_ \st -> st { tags = Set.delete tag st.tags }
      
      -- you can rely on the existing handleAction and handleQuery functions to recursively call 
      -- with further actions or queries.
      _ <- TagInput.handleQuery handleExtraActions (TagInput.SelectTag "my-auto-selected-tag" unit)
      TagInput.handleAction handleExtraActions (TagInput.handleInput "my-auto-selected-tag-1")

      -- you can also send queries through the renderless component to a child component you 
      -- placed in the render function
      H.query _annotation 0 (GetAnnotation identity) >>= case _ of
        Nothing -> log "no annotation at index 0"
        Just str -> log $ "annotation found at index 0: " <> str
      H.raise (tagRemoved tag)
  }

-- and now we'll write the render function for the extended component
renderTagInput
  :: forall m
   . MonadEffect m
  => TagInput.State 
  -> H.ComponentHTML (TagInput.Action InnerChildSlots m ExtraActions) InnerChildSlots m
renderTagInput st@{ tags } = 
  HH.div_ 
    [ HH.input $ TagInput.attachInputProps st [] 
    , HH.div_ $ Set.toUnfoldable tags # mapWithIndex \i x -> 
        HH.li 
          [ HE.onClick \_ -> Just (removeTag x) ] 
          [ HH.slot _annotation i innerComponent (x <> ".auto") absurd ]
    ]

-----
-- Component

-- Now we can make our new, extended tag input 

data Action 
  -- We will handle all messages already output by the component, plus our extra messages
  = HandleTagInput (TagInput.Message ExtraMessages)

type ChildSlots =
  ( tagInput :: TagInput.Slot InnerChildSlots ExtraMessages ExtraActions Unit )

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
      [ HH.slot _tagInput unit (TagInput.component handleExtraActions) (TagInput.Input { render: renderTagInput }) (Just <<< HandleTagInput) ]
  
  -- we can write our handler pretty much as usual, but because Message is a variant, we'll use `match` 
  -- instead of `case`. Notice how you can only use `Perform` to trigger your own provided actions, not
  -- just any action at all.
  handleAction :: _ -> H.HalogenM Unit _ _ _ m Unit
  handleAction = case _ of
    HandleTagInput msg -> msg # match
      { tagRemoved: \str -> do 
          log $ "Removed " <> str
      , tagAdded: \str -> do
          log $ "Added " <> str
          _ <- H.query _tagInput unit $ H.tell $ TagInput.Perform (removeTag str)

          -- you can send queries through two layers and get the result directly using `send`. as an
          -- alternative, if you want this managed by the renderless component, you should extend the
          -- algebra there with an action which sends and manages the result
          mbAnnotation <- TagInput.send _tagInput unit _annotation 0 (GetAnnotation identity)

          pure unit
      }
      
_tagInput = SProxy :: SProxy "tagInput"
_annotation = SProxy :: SProxy "annotation"

----------
-- A child component to be passed to the tag input

data InnerAction 
  = Receive String

data InnerQuery a
  = GetAnnotation (String -> a)

innerComponent :: ∀ m. MonadEffect m => H.Component HH.HTML InnerQuery String Void m
innerComponent =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ defaultEval
        { handleAction = handleAction 
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }
  where
  render :: String -> H.ComponentHTML InnerAction () m
  render str = 
    HH.span_ [ HH.text $ "ANN: " <> str ] 
  
  -- we can write our handler pretty much as usual, but because Message is a variant, we'll use `match` 
  -- instead of `case`. Notice how you can only use `Perform` to trigger your own provided actions, not
  -- just any action at all.
  handleAction = case _ of
    Receive str -> H.put str
  
  handleQuery :: forall a. InnerQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    GetAnnotation reply -> do
      st <- H.get
      pure $ Just $ reply st