module Main where

import Demo.Index qualified as Index
import Main.Utf8 (withUtf8)
import Network.Wai.Handler.Warp qualified as Warp
import Shroomz.Init (initialize)

main ∷ IO ()
main = withUtf8 do
  hSetBuffering stdout LineBuffering
  (port, waiApplication) ← initialize Index.app
  putTextLn $ "\nShroomz App is listening on http://localhost:" <> show port
  Warp.run port waiApplication
