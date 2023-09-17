module Launcher.Logic.Processes where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.Supply (freshId)
import Data.Time (getCurrentTime)
import Launcher.AppM (AppM, AppState (..), modifyAppStateM)
import Launcher.Data.Process (Process (..), ProcessId (..))
import System.Process.Typed
  ( getExitCode
  , inherit
  , setStdin
  , setStdout
  , startProcess
  )
import System.Process.Typed qualified as System

class Monad m ⇒ ProcessMgmt m where
  launchProcess ∷ m ProcessId
  listProcesses ∷ m [Process]
  stopProcess ∷ ProcessId → m ()
  removeExitedProcesses ∷ m ()

instance ProcessMgmt AppM where
  launchProcess ∷ AppM ProcessId
  launchProcess = modifyAppStateM \appState@AppState {..} → do
    let (ProcessId → processId, processIdSupply') = freshId processIdSupply
    process ← liftIO $ launchProcessIO processId
    pure
      ( appState
          { processIdSupply = processIdSupply'
          , processes = process : processes
          }
      , processId
      )

  listProcesses ∷ AppM [Process]
  listProcesses = gets processes

  stopProcess ∷ ProcessId → AppM ()
  stopProcess processId = modifyAppStateM \appState@AppState {..} → do
    processes' ← liftIO $ stopProcessIO processId processes
    pure (appState {processes = processes'}, ())

  removeExitedProcesses ∷ AppM ()
  removeExitedProcesses = modifyAppStateM \appState@AppState {..} → do
    processes' ← liftIO $ removeExitedProcessesIO processes
    pure (appState {processes = processes'}, ())

--------------------------------------------------------------------------------
-- IO functions ----------------------------------------------------------------

launchProcessIO ∷ ProcessId → IO Process
launchProcessIO processId = do
  let processConfig =
        System.proc command []
          & setStdin inherit
          & setStdout inherit
  processProcess ← startProcess processConfig
  processStarted ← getCurrentTime
  pure Process {processStopped = False, ..}

stopProcessIO ∷ ProcessId → [Process] → IO [Process]
stopProcessIO processIdToStop processes = do
  forConcurrently processes \process@Process {processId, processProcess} → do
    if processIdToStop == processId
      then System.stopProcess processProcess $> process {processStopped = True}
      else pure process

removeExitedProcessesIO ∷ [Process] → IO [Process]
removeExitedProcessesIO processes =
  forConcurrently processes \process@Process {processProcess} →
    getExitCode processProcess >>= \case
      Nothing → pure process
      Just _c →
        System.stopProcess processProcess
          $> process {processStopped = True}

command ∷ FilePath
command = "launcher/loop.sh"
