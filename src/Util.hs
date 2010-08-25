module Util where

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

simpleCGI :: (ToMessage a) => ServerPartT IO a -> IO ()
simpleCGI = runFastCGIConcurrent 10 . serverPartToCGI

escQuery :: [(String, String)] -> [Char]
escQuery = let esc = escapeURIString isUnreserved
  in intercalate "&" .  map (\ (key, val) -> esc key ++ "=" ++ esc val)

procQuery :: [(d, Input)] -> [(d, [Char])]
procQuery = map (second (map (chr . fromIntegral) . BS.unpack . inputValue))
