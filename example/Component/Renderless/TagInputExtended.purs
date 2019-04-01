module Example.Component.Renderless.TagInputExtended where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import Example.Component.Renderless.TagInput as TagInput
import Example.Component.Tag as Tag
import Halogen as H
import Renderless.State (getState, modifyState, putState)

-----
-- Extensions

-- You can extend a renderless component in three significant ways:
--   - you can add new actions to the component, if you provide a corresponding handler
--   - you can add your own output messages and handle them by pattern-matching on `Emit`
--   - you can render further child components and control them as usual

-- We're going to extend our basic tag input accordingly.
--  - we will add a new action which supports removing a tag
--  - we will add a new message which notifies users when a tag has been removed
--  - we will render individual tag components instead of simple strings and control them

-----
-- Extended Child Components

-- We are mounting a single type of child component within the renderless component: a simple
-- editable tag. We'll render this tag component instead of simple strings. Note: the renderless
-- component is unaware of this tag component! You could substitute any component or multiple 
-- types of components as you see fit.

type TagInputChildSlots = ( tag :: H.Slot Tag.Query Tag.Message Int )

_tag = SProxy :: SProxy "tag"

-----
-- Extended Messages

-- We need one new message for our tag input component: we'll emit a "tagRemoved" message 
-- when a tag has been removed from the tag list.

type ExtraMessages = 
  ( tagRemoved :: String )

tagRemoved :: String -> TagInput.Message ExtraMessages
tagRemoved = inj (SProxy :: _ "tagRemoved") 

-----
-- Extended Actions

-- We need two new actions for our tag input component: the ability to remove a tag and
-- the ability to handle messages emitted from our individual tag components.

type ExtraActions = 
  ( removeTag :: String 
  , handleTag :: Tag.Message
  )

removeTag :: forall v. String -> Variant (removeTag :: String | v)
removeTag = inj (SProxy :: _ "removeTag") 

handleTag :: forall v. Tag.Message -> Variant (handleTag :: Tag.Message | v)
handleTag = inj (SProxy :: _ "handleTag") 

-- Now that we have two new actions with which to extend our renderless tag input, we'll need to 
-- provide a handler that will deal with these actions when they arise.

-- Admittedly, the type signatures can get lengthy. This will trim things down slightly:
type HandleExtraActions slots actions out m =
  H.HalogenM (TagInput.StateStore slots m actions) (TagInput.Action slots m actions) slots (TagInput.Message out) m Unit

-- Our handler will deal with just the actions we are extending the component with. We're using variants,
-- so we'll use `match` instead of `case`.
handleExtraActions 
  :: forall m
   . MonadEffect m
  => Variant ExtraActions 
  -> HandleExtraActions TagInputChildSlots ExtraActions ExtraMessages m
handleExtraActions = match
  -- When a tag is removed, we want to filter it out of the tags available in state.
  { removeTag: \tag -> do
      oldState <- modifyState \st -> st { tags = Array.filter (_ /= tag) st.tags }

      -- All we really need to do is filter the tag out of state. But just for fun, we can recursively 
      -- call the existing renderless component handleQuery and handleAction functions, or this 
      -- handleExtraActions function we're writing now. 
      _ <- TagInput.handleQuery handleExtraActions (TagInput.SelectTag "you-deleted-a-tag" unit)
      TagInput.handleAction handleExtraActions (TagInput.handleInput "you-deleted-a-tag")

      -- We can verify things did change:
      st <- getState
      logShow st.tags
      logShow st.text

      -- And then replace the previous state.
      putState oldState

      -- Then, we'll raise our new message, `tagRemoved`, to indicate a tag has been removed.
      H.raise (tagRemoved tag)

  -- When a message is emitted by a child component, we ought to handle it.
  , handleTag: case _ of
      Tag.Delete index -> do
        -- We can query child components as usual. Here, we'll look up the original value of the tag
        -- at the index that ought to be deleted, and then recursively call handleExtraActions to
        -- remove it. 
        H.query _tag index (Tag.GetInitialTag identity) >>= case _ of
          Nothing -> 
            log $ "no tag at index " <> show index
          Just str -> 
            handleExtraActions (removeTag str)
  }


-----
-- EXTENDED COMPONENT

-- At the end of the day, after all this extension, the end user will still be using our original
-- renderless component, just with our various extensions in place as well. See the `Page.purs` file 
-- for an example of using both the original and the extended components.