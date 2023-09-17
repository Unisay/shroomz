module Launcher.Shroom.Processes where

import Data.Time (defaultTimeLocale, formatTime, rfc822DateFormat)
import Launcher.Data.Process (Process (..), ProcessId)
import Launcher.Logic.Processes (ProcessMgmt (..), command)
import Lucid.Extended
import Shroomz.Component
  ( Component (..)
  , ComponentData (..)
  , parseActionField
  )
import Shroomz.Component.Path (ComponentPath)
import Web.HttpApiData (toUrlPiece)
import Prelude hiding (State)

data Action
  = Launch
  | Stop ProcessId
  | Refresh
  deriving stock (Show, Read)

type State = [Process]

component ∷ ProcessMgmt m ⇒ Component m State Action
component = Component {parseAction = parseActionField "action", ..}
 where
  initialise ∷ Applicative m ⇒ m (ComponentData m State)
  initialise = pure ComponentData {children = mempty, userState = mempty}

  update ∷ ProcessMgmt m ⇒ State → Action → m State
  update _processes action =
    listProcesses <* case action of
      Launch → launchProcess *> removeExitedProcesses
      Stop processId → stopProcess processId
      Refresh → removeExitedProcesses

  render ∷ ComponentPath → State → renderChild → Html_
  render path processes _renderChild = do
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

processName ∷ Process → String
processName = formatTime defaultTimeLocale rfc822DateFormat . processStarted
