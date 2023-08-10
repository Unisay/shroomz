module Main where

import App qualified (serveApi)
import Main.Utf8 (withUtf8)

main ∷ IO ()
main = withUtf8 do
  hSetBuffering stdout LineBuffering
  let port = 3000
  putText $ "Listening on port " <> show port <> "..."
  hFlush stdout
  App.serveApi port
