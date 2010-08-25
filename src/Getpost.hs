import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Monad.Error
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans
import Data.Array hiding ((!))
import Data.Char
import Data.List
import Data.Maybe
import FUtil
import Happstack.Server hiding (method)
import Happstack.Server.FastCGI
import System.Directory
import System.Environment
import System.FilePath
import System.Console.GetOpt
import Text.HTML.TagSoup
import Text.XHtml hiding (dir)
import Network.URI
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Network.HTTP as H

import Util

main :: IO ()
main = simpleCGI $ withRequest getpost

getpost req = ok . toResponse . concatHtml $
  if null inps
    then [
      toHtml "drag this link to your bookmarks bar:",
      br,
      anchor (toHtml "getpost") ! [strAttr "onclick" "return false;", href js],
      br,
      toHtml "then click it on a page with a form to make that form submit to \
             \a link that enables you to share the post as a get link"
      ]
    else [
      toHtml "The getpost link is:",
      br,
      anchor (toHtml ustr) ! [href ustr]
      ]
  where
  inps = procQuery $ rqInputs req
  qstr = escQuery inps
  ustr = show $
    URI "http:" (Just $ URIAuth "" "dzl.no-ip.org" "") "/dopost?" qstr ""
  js = "javascript:\
\form = document.forms[0];\
\action = form.action;\
\if (action.indexOf(':') == -1) {\
\  href = location.href;\
\  i = href.lastIndexOf('/');\
\  action = href.slice(0, i + 1) + action;\
\}\
\form.action = 'http://dzl.no-ip.org/getpost?orig_action=' + \
\  encodeURIComponent(action);\
\els = form.elements;\
\for (i = 0; i < els.length; i++) {\
\  if (els[i].type == 'submit') {\
\    el = els[i];\
\    break;\
\  }\
\}\
\if (el) {\
\  el.value += ' -- getpost'\
\}\
\void(0);"

