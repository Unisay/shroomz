module Launcher.Data.Process where

import Data.Time (UTCTime)
import System.Process.Typed qualified as Sys

newtype ProcessId = ProcessId Int
  deriving newtype (Show, Read, Eq)

data Process = Process
  { processId ∷ ProcessId
  , processProcess ∷ Sys.Process () () ()
  , processStarted ∷ UTCTime
  , processStopped ∷ Bool
  }
