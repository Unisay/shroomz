module Launcher.AppM where

import Control.Concurrent.Supply (Supply)
import Launcher.Data.Process (Process)
import UnliftIO (MonadUnliftIO)

data AppState = AppState
  { processes ∷ [Process]
  , processIdSupply ∷ Supply
  }

newtype AppM a = AppM (ReaderT (IORef AppState) IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    )

runAppM ∷ AppState → AppM a → IO a
runAppM s (AppM r) = runReaderT r =<< newIORef s

instance MonadState AppState AppM where
  get = AppM $ readIORef =<< ask

  put s = AppM $ (`writeIORef` s) =<< ask

  state f = AppM do
    ref ← ask
    atomicModifyIORef' ref (swap . f)

modifyAppStateM ∷ (AppState → AppM (AppState, a)) → AppM a
modifyAppStateM f = do
  s ← get
  (s', a) ← f s
  put s'
  pure a
