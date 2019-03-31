-- | A classic tag input
module Example.Classic where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match)
import Effect.Class (class MonadEffect)
import Example.Renderless.TagInput as TagInput
import Halogen (defaultEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Renderless.State (modifyState_)

type ChildSlots =
  ( tagInput :: TagInput.Slot (Const Void) () Unit )

component :: ∀ m. MonadEffect m => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval defaultEval
    }
  where
  render :: Unit -> H.ComponentHTML Void ChildSlots m
  render _ = 
    HH.div_ 
      [ HH.slot (SProxy :: _ "tagInput") unit (TagInput.component handleExtraActions) { render: renderTagInput } (\_ -> Nothing) ]

  renderTagInput
    :: TagInput.State 
    -> H.ComponentHTML (TagInput.Action (Const Void) () m ExtraActions) () m
  renderTagInput st@{ tags } = 
    HH.div_ 
      [ HH.input $ TagInput.attachInputProps st [] 
      , HH.div_ $ Set.toUnfoldable tags # map \x -> 
          HH.li 
            [ HE.onClick \_ -> Just (removeTag x) ] 
            [ HH.text x ]
      ]

  removeTag :: String -> TagInput.Action (Const Void) () m ExtraActions
  removeTag = TagInput.Action <<< inj (SProxy :: _ "removeTag") 
 

type HandleExtraActions pq ps m v = 
  H.HalogenM (TagInput.StateStore pq ps m v) (TagInput.Action pq ps m v) ps (TagInput.Message pq) m Unit

type ExtraActions = 
  ( removeTag :: String )

handleExtraActions 
  :: forall m
   . Variant ExtraActions 
  -> HandleExtraActions (Const Void) () m ExtraActions
handleExtraActions = match
  { removeTag: \tag ->
      modifyState_ \st -> st { tags = Set.delete tag st.tags }
  }