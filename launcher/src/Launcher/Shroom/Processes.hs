module Launcher.Shroom.Processes where

import Data.Foldable (foldrM)
import Data.Map.Strict qualified as Map
import Data.Time (defaultTimeLocale, formatTime, rfc822DateFormat)
import Launcher.Data.Process (Process (..), ProcessId)
import Launcher.Logic.Processes (ProcessEditor (..), ProcessViewer (..), command)
import Lucid.Extended
  ( Html_
  , ToHtml (toHtml)
  , a_
  , class_
  , div_
  , hxAction_
  , hxPost_
  , hxSwap_
  , hxTarget_
  , hxTrigger_
  , ionIcon_
  , table_
  , tbody_
  , td_
  , th_
  , thead_
  , tr_
  )
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

type State = Map ProcessId ProcessView

data ProcessView = ProcessView
  { name ∷ Text
  , hasStopped ∷ Bool
  , tailLogLines ∷ [Text]
  }
  deriving stock (Show)

component ∷ ∀ m. ProcessEditor m ⇒ Component m State Action
component = Component {parseAction = parseActionField "action", ..}
 where
  initialise ∷ Applicative m ⇒ m (ComponentData m State)
  initialise = pure ComponentData {children = mempty, userState = mempty}

  update ∷ ProcessEditor m ⇒ State → Action → m State
  update processViews action = do
    case action of
      Launch → launchProcess *> removeExitedProcesses
      Stop pid → stopProcess pid
      Refresh → removeExitedProcesses
    processes ← listProcesses
    foldrM updateViews processViews processes
   where
    updateViews ∷ Process → State → m State
    updateViews process views = do
      let pid = processId process
      case Map.lookup pid views of
        Nothing → updateProcessView pid process views
        Just oldView →
          if hasStopped oldView
            then pure views
            else updateProcessView pid process views

    updateProcessView ∷ ProcessId → Process → State → m State
    updateProcessView pid process views = do
      tailLogLines ← tailProcessLog 60 process
      pure $
        views
          & Map.insert
            pid
            ProcessView
              { name =
                  toText $
                    formatTime
                      defaultTimeLocale
                      rfc822DateFormat
                      (processStarted process)
              , hasStopped = processStopped process
              , tailLogLines
              }

  render ∷ ComponentPath → State → renderChild → Html_
  render path processes _renderChild = do
    div_
      ( hxPost_ (toUrlPiece path)
          : hxSwap_ "outerHTML"
          : hxTarget_ "this"
          : hxAction_ Refresh
          : [hxTrigger_ "load delay:2s" | not (all hasStopped processes)]
      )
      do
        table_ [class_ "table is-fullwidth is-hoverable"] do
          thead_ do
            tr_ do
              th_ do "PID"
              th_ do "Started"
              th_ do "Command"
              th_ do "Stopped"
              th_ do "Log"
          tbody_ do
            forM_ (Map.toList processes) \(pid, process) → tr_ do
              td_ do show pid
              td_ do toHtml (name process)
              td_ do toHtml command
              td_ do
                let icon s =
                      ionIcon_
                        (s <> "-circle-outline")
                        (class_ "icon")
                if hasStopped process
                  then icon "checkmark"
                  else
                    a_
                      [ hxPost_ (toUrlPiece path)
                      , hxTrigger_ "click"
                      , hxAction_ (Stop pid)
                      ]
                      (icon "stop")
              td_ do toHtml $ fromMaybe "" $ viaNonEmpty head (tailLogLines process)
