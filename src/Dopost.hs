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
main = simpleCGI $ withRequest dopost

dopost req = ok . toResponse $ concatHtml [
  toHtml "posting..",
  form hids ! [method "POST", action origAction],
  script $ toHtml "document.forms[0].submit()"
  ] where
  inps = procQuery $ rqInputs req
  (origActionL, inps') = partition ((== "orig_action") . fst) inps
  -- FIXME: error better?
  origAction = snd $ head origActionL
  hids = concatHtml $ map (uncurry hidden) inps'

