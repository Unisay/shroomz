module Launcher.Data.Process where

import Data.Time (UTCTime)
import Path (Abs, File, Path)
import System.Process.Typed qualified as Sys

newtype ProcessId = ProcessId Int
  deriving newtype (Show, Read, Eq, Ord)

data Process = Process
  { processId ∷ ProcessId
  , processProcess ∷ Sys.Process () () ()
  , processStarted ∷ UTCTime
  , processStopped ∷ Bool
  , processLogFile ∷ (Path Abs File, Maybe Handle)
  }
  deriving stock (Show)
