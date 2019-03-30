-- | A classic tag input
module Example.Classic where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Example.Renderless.TagInput as TagInput
import Halogen (defaultEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type ChildSlots =
  ( inner :: TagInput.Slot (Const Void) () Unit )

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
      [ HH.slot (SProxy :: SProxy "inner") unit TagInput.component { render: render' } (\_ -> Nothing)
      ]

  render' 
    :: TagInput.State 
    -> H.ComponentHTML (TagInput.Action (Const Void) () m) () m
  render' st@{ tags } = 
    HH.div_ 
      [ HH.input $ TagInput.attachInputProps st [] 
      , HH.div_ $ Set.toUnfoldable tags # map \x -> 
          HH.li 
            [ HE.onClick \_ -> Just (TagInput.RemoveTag x) ] 
            [ HH.text x ]
      ]