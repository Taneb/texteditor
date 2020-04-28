Text editor notes
=================

> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NamedFieldPuns #-}
> module Main where

> import Control.Concurrent
> import Control.Exception
> import Control.Lens
> import Control.Monad.State
> import Data.Foldable
> import Data.Text (Text)
> import qualified Data.Text as T
> import qualified Data.Text.IO as T

We want to be the XMonad of text editors!

Highly configurable, config file is the editor.

As small components as possible! Like, there is a component which just holds
the current file name, and the component to save the file queries that and the
text buffer.

Components can:
* do arbitrary IO (necessary for a lot of functionality! Although it should be
  discouraged)
* create more components
* send messages to other components (or themselves, I guess)

Initially I want to try and rely on BlockedIndefinitelyOnMVar to handle when to
clear a component. This will happen when it is waiting for a command but no
components are going to send them. This means that generally components
shouldn't be able to send messages to themselves, unless they intend to last
forever (or can drop the channel).

What should a component look like? Probably something like

> data Component init msg = forall s. Component
>    { initComponent :: init -> IO s
>    , handleMsgComponent :: msg -> StateT s IO ()
>    }

Alternatively, we can drop the StateT and s and store data in an IORef.

If a message requires a response, it can contain a function (rsp -> StateT s'
IO ()) or something similar within it. This will then be invoked by the
component.

For suitable 'init' and 'msg' types.

I don't know if 'init' is necessary, we could have a function to Component
instead! Although maybe we want to do some action on startup or create state.
I'll leave it as is, I think.

This design I think precludes or makes difficult timer-like components,
although maybe not.

We wrap this up in a thing that sets up a new thread and returns a Chan msg
other components can use to communicate with the component.

> launchComponent :: Component init msg -> init -> IO (Chan msg)
> launchComponent Component { initComponent, handleMsgComponent } i = do
>   msgChan <- newChan
>   initialState <- initComponent i
>   _ <- forkIO $
>     evalStateT (forever (componentLoop msgChan)) initialState
>       `catch` \BlockedIndefinitelyOnMVar -> pure ()
>   pure msgChan
>   where
>     componentLoop msgChan = do
>       next <- liftIO $ readChan msgChan
>       handleMsgComponent next

Necessary components
--------------------

Text buffer

This requires no initialization, and stores a list of lines.

Storing a list of lines makes it simpler to work with deltas rather than the
whole block, which will save memory for large files. For now we don't bother
with that, though.

We want to be able to update the text. This can be a function [Text] -> [Text].

We also want to be able to add a handler for when the text gets updated! We can
use this to automatically redraw the screen or to autosave on write or even for
IDE-like functionality! This means we wan to also carry around a list of
handlers.

And finally we want to be able to ask for the whole text when we want it, for
saving or something.

> data TextMsg
>   = UpdateText ([Text] -> [Text])
>   | RegisterUpdateHandler ([Text] -> IO ())
>   | GetText ([Text] -> IO ())

> textBuffer :: Component () TextMsg
> textBuffer = Component
>   { initComponent = \() -> pure ([], [])
>   , handleMsgComponent = \case
>     UpdateText f -> do
>       newText <- _1 <%= f
>       handlers <- gets snd
>       liftIO $ traverse_ ($ newText) handlers
>     RegisterUpdateHandler handler -> _2 %= (:) handler
>     GetText f -> join $ uses _1 (liftIO . f)
>   }

File location

We want to keep a record of where a given buffer is on disk. This is useful for
saving the file, for reloading the file if it's been changed by another
process, and for informational displays.

It really just needs to store the filepath and little else. We can make this
more general by parametrizing it to store any type!

> data SimpleStoreMsg a
>   = Replace a
>   | Get (a -> IO ())

> simpleStore :: Component a (SimpleStoreMsg a)
> simpleStore = Component
>   { initComponent = pure
>   , handleMsgComponent = \case
>     Replace x -> put x
>     Get cb -> join $ gets (liftIO . cb)
>   }

We need a way to save files. This will recieve an instruction to save, and then
query both the simpleStore containing the file handle, and the text buffer, and
save the file.

> fileSave
>   :: Chan (SimpleStoreMsg (Maybe FilePath))
>   -> Chan TextMsg
>   -> Component () ()
> fileSave fs tb = Component
>   { initComponent = pure
>   , handleMsgComponent = \() ->
>       liftIO . writeChan fs . Get $ \case
>         Nothing -> pure () -- should this error?
>         Just p -> liftIO . writeChan tb . GetText $ T.writeFile p . T.unlines
>   }

> main :: IO ()
> main = pure ()
