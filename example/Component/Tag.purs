module Example.Component.Tag where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent as KE

-- This simple component represents an individual tag, which can be edited freely while
-- retaining its original label. Components can't delete themselves, so this component
-- emits a message when the tag is removed. It is not a renderless component.

type State = 
  { editing :: Boolean 
  , currentTag :: String
  , initialTag :: String
  , index :: Int
  }

data Action 
  = HandleTextInput String
  | HandleKey KE.KeyboardEvent
  | HandleSave
  | HandleEdit
  | HandleDelete
  | Receive Input

data Query a
  = GetInitialTag (String -> a)
  | GetTagIndex (Int -> a)

data Message = Delete Int

type Input =
  { initialTag :: String 
  , index :: Int 
  }

component :: ∀ m. MonadEffect m => H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: \{ initialTag, index } -> { editing: false, currentTag: initialTag, initialTag, index }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction 
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render { currentTag, editing } = unit # case editing of
    true -> \_ ->
      HH.div_
        [ HH.input 
            [ HP.value currentTag
            , HE.onKeyDown $ Just <<< HandleKey
            , HE.onValueInput $ Just <<< HandleTextInput
            ]
        , HH.button
            [ HE.onClick \_ -> Just HandleSave ]
            [ HH.text "Save" ]
        , HH.button
            [ HE.onClick \_ -> Just HandleDelete ]
            [ HH.text "Delete" ]
        ]
    _ -> \_ ->
      HH.span 
        [ HE.onClick \_ -> Just HandleEdit ] 
        [ HH.text $ "<" <> currentTag <> ">" ] 
  
  handleAction = case _ of
    HandleTextInput str -> 
      H.modify_ _ { currentTag = str }
    HandleKey ev -> when (KE.code ev == "Enter") do
      H.liftEffect $ preventDefault (KE.toEvent ev)
      handleAction HandleSave
    HandleSave -> 
      H.modify_ _ { editing = false }
    HandleEdit -> 
      H.modify_ _ { editing = true }
    HandleDelete -> 
      H.get >>= \st -> H.raise $ Delete st.index
    Receive { index, initialTag } -> 
      H.modify_ _ { index = index, initialTag = initialTag }
  
  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    GetInitialTag reply -> do
      H.get >>= \st -> pure $ Just $ reply st.initialTag
    GetTagIndex reply -> do
      H.get >>= \st -> pure $ Just $ reply st.index