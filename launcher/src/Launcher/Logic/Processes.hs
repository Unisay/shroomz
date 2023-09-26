module Launcher.Logic.Processes where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.Supply (freshId)
import Data.Time (getCurrentTime)
import Launcher.AppM (AppM, AppState (..), modifyAppStateM)
import Launcher.Data.Process (Process (..), ProcessId (..))
import Path (parseRelFile, toFilePath, (</>))
import Path.IO (getCurrentDir)
import System.IO
  ( SeekMode (SeekFromEnd)
  , hGetChar
  , hSeek
  )
import System.IO.Utf8 (openFile, withFile)
import System.Process.Typed
  ( getExitCode
  , inherit
  , setStdin
  , setStdout
  , startProcess
  , useHandleClose
  )
import System.Process.Typed qualified as System
import Prelude hiding (withFile)

class Monad m ⇒ ProcessViewer m where
  listProcesses ∷ m [Process]
  tailProcessLog ∷ Natural → Process → m [Text]

instance ProcessViewer AppM where
  listProcesses = gets processes
  tailProcessLog n Process {processLogFile = (logFilePath, handle)} =
    liftIO case handle of
      Nothing → withFile (toFilePath logFilePath) ReadMode (tailFile n)
      Just hdl → tailFile n hdl

class ProcessViewer m ⇒ ProcessEditor m where
  launchProcess ∷ m ProcessId
  stopProcess ∷ ProcessId → m ()
  removeExitedProcesses ∷ m ()

instance ProcessEditor AppM where
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
  logFileName ← parseRelFile $ "process-" <> show processId <> ".log"
  logFilePath ← getCurrentDir <&> (</> logFileName)
  handle ← openFile (toFilePath logFilePath) ReadWriteMode
  let stream = useHandleClose handle
  let processConfig =
        System.proc command []
          & setStdin inherit
          & setStdout stream
  processProcess ← startProcess processConfig
  processStarted ← getCurrentTime
  pure
    Process
      { processStopped = False
      , processLogFile = (logFilePath, Just handle)
      , ..
      }

tailFile ∷ Natural → Handle → IO [Text]
tailFile n hdl = do
  hSeek hdl SeekFromEnd (-fromIntegral n)
  lines . toText <$> replicateM (fromIntegral n) (hGetChar hdl)

stopProcessIO ∷ ProcessId → [Process] → IO [Process]
stopProcessIO processIdToStop processes = do
  forConcurrently processes \process@Process {processId, processProcess} → do
    if processIdToStop == processId
      then
        System.stopProcess processProcess
          $> process
            { processStopped = True
            , processLogFile = processLogFile process $> Nothing
            }
      else pure process

removeExitedProcessesIO ∷ [Process] → IO [Process]
removeExitedProcessesIO processes =
  forConcurrently processes \process@Process {processProcess} →
    getExitCode processProcess >>= \case
      Nothing → pure process
      Just _c →
        System.stopProcess processProcess
          $> process
            { processStopped = True
            , processLogFile = processLogFile process $> Nothing
            }

command ∷ FilePath
command = "launcher/loop.sh"
