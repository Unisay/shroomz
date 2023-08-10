module App (serveApi) where

import Html (HTML)
import Layout qualified
import Lucid.Extended (Html_)
import Network.Wai.Handler.Warp qualified as Warp
import Page.Docs qualified as Docs
import Page.Home qualified as Home
import Servant qualified
import Servant.API (Get, Raw, type (:<|>) (..), type (:>))
import Servant.Server qualified as Server
import Servant.Server.StaticFiles (serveDirectoryWebApp)

type Layout = Get '[HTML] Html_ :<|> Pages

serveLayout ∷ Server.Handler Html_
serveLayout = pure $ Layout.html Home.html

type Pages = "page" :> (Home :<|> Docs)

type Home = "home" :> Get '[HTML] Html_

serveHome ∷ Server.Handler Html_
serveHome = pure Home.html

type Docs = "docs" :> Get '[HTML] Html_

serveDocs ∷ Server.Handler Html_
serveDocs = pure Docs.html

type Static = "static" :> Raw

serveStatic ∷ Servant.ServerT Raw m
serveStatic = serveDirectoryWebApp "static"

type Api = Layout :<|> Static

serveApi ∷ Int → IO ()
serveApi port =
  Warp.run port $
    Server.serve (Proxy @Api) $
      (serveLayout :<|> serveHome :<|> serveDocs) :<|> serveStatic
