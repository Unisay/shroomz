module Shroomz.Dev where

import Control.Concurrent
  ( ThreadId
  , forkFinally
  , killThread
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Demo.Index qualified as Index
import Foreign.Store
  ( Store (..)
  , lookupStore
  , readStore
  , storeAction
  , withStore
  )
import GHC.IO.Handle (hIsTerminalDevice)
import Network.Wai.Handler.Warp qualified as Warp
import Shroomz.Init (initialize)
import Prelude hiding (newEmptyMVar, putMVar, takeMVar)

{- | Start or restart the server.
newStore is from foreign-store.
A Store holds onto some data across ghci reloads
-}
update ∷ IO ()
update = do
  lookupStore storeId >>= \case
    -- no server running
    Nothing → do
      putStrLn "Dev server is starting..."
      done ← storeAction doneStore newEmptyMVar
      tid ← start done
      _ ← storeAction (Store storeId) (newIORef tid)
      putStrLn "Dev server has started."
    -- server is already running
    Just tidStore → do
      putStrLn "Dev server is restarting..."
      restartAppInNewThread tidStore
      putStrLn "Dev server has restarted."
 where
  doneStore ∷ Store (MVar Done)
  doneStore = Store 0

  -- shut the server down with killThread and wait for the done signal
  restartAppInNewThread ∷ Store (IORef ThreadId) → IO ()
  restartAppInNewThread tidStore = modifyStoredIORef tidStore \tid → do
    killThread tid
    _done ← withStore doneStore takeMVar
    readStore doneStore >>= start

  -- Start the server in a separate thread.
  start ∷ MVar Done → IO ThreadId
  start done = do
    terminal ← hIsTerminalDevice stdout
    putStrLn $ "is terminal: " <> show terminal
    (port, app) ← initialize Index.app
    let run = do
          isTerminal ← hIsTerminalDevice stdout
          if not isTerminal then error "stdout is not a terminal" else pass
          Warp.run port app
    forkFinally run \_ → putMVar done Done

-- | kill the server
shutdown ∷ IO ()
shutdown =
  lookupStore storeId >>= \case
    -- no server running
    Nothing → putStrLn "no application running"
    Just tidStore → do
      withStore tidStore $ readIORef >=> killThread
      putStrLn "Dev server has been shut down."

storeId ∷ Word32
storeId = 1

modifyStoredIORef ∷ Store (IORef a) → (a → IO a) → IO ()
modifyStoredIORef store f = withStore store \ref → do
  v ← readIORef ref
  f v >>= writeIORef ref

data Done = Done
