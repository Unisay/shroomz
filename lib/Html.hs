module Html where

import Lucid (ToHtml (..), renderBS)
import Network.HTTP.Media.MediaType ((//), (/:))
import Servant (Accept (..), MimeRender (..))

data HTML deriving stock (Typeable)

-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentTypes _ =
    "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]

instance ToHtml a ⇒ MimeRender HTML a where
  mimeRender _ = renderBS . toHtml
