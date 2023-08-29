module Main where

import Main.Utf8 (withUtf8)
import Network.Wai.Handler.Warp qualified as Warp
import Shroomz.Init (initialize)

main ∷ IO ()
main = withUtf8 do
  hSetBuffering stdout LineBuffering
  hFlush stdout
  (port, waiApplication) ← initialize
  putTextLn $ "Serving on port " <> show port
  Warp.run port waiApplication
