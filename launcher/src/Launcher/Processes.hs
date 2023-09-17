module Launcher.Processes where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.Supply (Supply, freshId, newSupply)
import Data.Time
  ( UTCTime
  , defaultTimeLocale
  , formatTime
  , getCurrentTime
  , rfc822DateFormat
  )
import Lucid.Extended
import Shroomz.Component
  ( Component (..)
  , ComponentData (..)
  , parseActionField
  )
import Shroomz.Component.Path (ComponentPath)
import System.Process.Typed
  ( getExitCode
  , inherit
  , setStdin
  , setStdout
  , startProcess
  )
import System.Process.Typed qualified as System
import Web.HttpApiData (toUrlPiece)
import Prelude hiding (State)

data Action = Launch | Stop ProcessId | Refresh
  deriving stock (Show, Read)

data State = LauncherState {idSupply ∷ Supply, processes ∷ [Process]}

newtype ProcessId = ProcessId Int
  deriving newtype (Show, Read, Eq)

component ∷ ∀ m. MonadIO m ⇒ Component m State Action
component = Component {parseAction = parseActionField "action", ..}
 where
  initialise ∷ m (ComponentData m State)
  initialise = do
    idSupply ← liftIO newSupply
    pure
      ComponentData
        { children = mempty
        , userState = LauncherState idSupply []
        }

  update ∷ State → Action → m State
  update st@(LauncherState idSupply processes) =
    liftIO . \case
      Launch → do
        let (processId, idSupply') = freshId idSupply
        process ← launchProcess (ProcessId processId)
        processes' ← removeExitedProcesses processes
        pure st {idSupply = idSupply', processes = process : processes'}
      Stop processId → do
        processes' ← stopProcess processId processes
        pure st {processes = processes'}
      Refresh → do
        processes' ← removeExitedProcesses processes
        pure st {processes = processes'}

  render ∷ ComponentPath → State → renderChild → Html_
  render path LauncherState {processes} _renderChild = do
    div_
      ( hxPost_ (toUrlPiece path)
          : hxSwap_ "outerHTML"
          : hxTarget_ "this"
          : hxAction_ Refresh
          : [hxTrigger_ "load delay:2s" | not (all processStopped processes)]
      )
      do
        table_ [class_ "table is-fullwidth is-hoverable"] do
          thead_ do
            tr_ do
              th_ do "PID"
              th_ do "Started"
              th_ do "Stopped"
              th_ do "Command"
          tbody_ do
            forM_ processes \process → tr_ do
              td_ do show (processId process)
              td_ do toHtml (processName process)
              td_ do
                let icon s =
                      ionIcon_
                        (s <> "-circle-outline")
                        (class_ "icon")
                if processStopped process
                  then icon "checkmark"
                  else
                    a_
                      [ hxPost_ (toUrlPiece path)
                      , hxTrigger_ "click"
                      , hxAction_ (Stop (processId process))
                      ]
                      (icon "stop")
              td_ do toHtml command

command ∷ FilePath
command = "launcher/loop.sh"

data Process = Process
  { processId ∷ ProcessId
  , processProcess ∷ System.Process () () ()
  , processStarted ∷ UTCTime
  , processStopped ∷ Bool
  }

processName ∷ Process → String
processName = formatTime defaultTimeLocale rfc822DateFormat . processStarted

launchProcess ∷ ProcessId → IO Process
launchProcess processId = do
  let processConfig =
        System.proc command []
          & setStdin inherit
          & setStdout inherit
  processProcess ← startProcess processConfig
  processStarted ← getCurrentTime
  pure Process {processStopped = False, ..}

stopProcess ∷ ProcessId → [Process] → IO [Process]
stopProcess processIdToStop processes = do
  forConcurrently processes \process@Process {processId, processProcess} → do
    if processIdToStop == processId
      then System.stopProcess processProcess $> process {processStopped = True}
      else pure process

removeExitedProcesses ∷ [Process] → IO [Process]
removeExitedProcesses processes =
  forConcurrently processes \process@Process {processProcess} →
    getExitCode processProcess >>= \case
      Nothing → pure process
      Just _c →
        System.stopProcess processProcess
          $> process {processStopped = True}
