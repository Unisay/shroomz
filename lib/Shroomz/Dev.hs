module Shroomz.Dev where

import Control.Concurrent
  ( ThreadId
  , forkFinally
  , killThread
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Foreign.Store
  ( Store (..)
  , lookupStore
  , readStore
  , storeAction
  , withStore
  )
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
  doneStore ∷ Store (MVar ())
  doneStore = Store 0

  -- shut the server down with killThread and wait for the done signal
  restartAppInNewThread ∷ Store (IORef ThreadId) → IO ()
  restartAppInNewThread tidStore = modifyStoredIORef tidStore \tid → do
    killThread tid
    withStore doneStore takeMVar
    readStore doneStore >>= start

  -- Start the server in a separate thread.
  start
    ∷ MVar ()
    -- \^ Written to when the thread is killed.
    → IO ThreadId
  start done = do
    (port, app) ← initialize
    forkFinally
      (Warp.runSettings (Warp.setPort port Warp.defaultSettings) app)
      -- Note that this implies concurrency
      -- between shutdownApp and the next app that is starting.
      -- Normally this should be fine
      \_ → putMVar done ()

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
