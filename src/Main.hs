import Control.Monad.Trans
import Control.Arrow hiding ((+++))
import Data.Char
import Data.List
import Data.Maybe
import HAppS.Server hiding (method)
import HTTP.FileServe
import System.Directory
import System.Environment
import System.FilePath
import System.Console.GetOpt
import Text.XHtml hiding (dir)
import Network.URI
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS

data Opts = Opts {
  optPort :: Int
  }

defOpts = Opts {
  optPort = 80
  }

progOpts = [
  Option ['p'] ["port"]
    (ReqArg (\ port opts -> opts {optPort = read port}) "PORT")
    "PORT to serve on"
  ]

dirify :: FilePath -> [String] -> Html
dirify dir fs = concatHtml $ map (\d -> toHtml
  (HotLink (joinPath ["/", dir, d]) (toHtml d) []) +++ br) $
  sort fs

myMimeTypes :: Map.Map [Char] [Char]
myMimeTypes = Map.insert "mem" "text/plain; charset=utf-8" mimeTypes

rootOrServe :: (MonadIO m) => Request -> WebT m Response
rootOrServe req =  if rqPaths req == []
  then fileServe' "index.html" fdir myMimeTypes req
  else fileServe' "." fdir myMimeTypes req where
      fdir _mime _rq fp = do
        contents <- liftIO $ getDirectoryContents fp
        return $ toResponse $ dirify fp $ filter (`notElem` ["."] ++
          (if fp == "." then [".."] else [])) $ contents

escQuery :: [(String, String)] -> [Char]
escQuery = let esc = escapeURIString isUnreserved
  in intercalate "&" .  map (\ (key, val) -> esc key ++ "=" ++ esc val)

procQuery :: [(d, Input)] -> [(d, [Char])]
procQuery = map (second (map (chr . fromIntegral) . BS.unpack . inputValue))

getpost :: (Monad m) => Request -> WebT m Response
getpost req = let
  inps = procQuery $ rqInputs req
  qstr = escQuery inps
  ustr = show $
    URI "http:" (Just $ URIAuth "" "dzl.no-ip.org" "") "/dopost?" qstr ""
  js = "javascript:\
\form = document.forms[0];\
\form.action = 'http://dzl.no-ip.org/getpost?orig_action=' + \
\  encodeURIComponent(form.action);\
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
  in ok . toResponse . concatHtml $ if null inps
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

dopost :: (Monad m) => Request -> WebT m Response
dopost req = let
  inps = procQuery $ rqInputs req
  (origActionL, inps') = partition ((== "orig_action") . fst) inps
  -- FIXME: error better?
  origAction = snd $ head origActionL
  hids = concatHtml $ map (uncurry hidden) inps'
  --in ok . toResponse $ 
  in ok . toResponse $ concatHtml [
    toHtml "posting..",
    form hids ! [method "POST", action origAction],
    script $ toHtml "document.forms[0].submit()"
    ]

main :: IO ()
main = do
  --print myMimeTypes
  args <- getArgs
  let header = "lol"
  (opts, []) <- case getOpt Permute progOpts args of
    (o, n, []) -> return (foldl (flip id) defOpts o, n)
    (_, _, errs) -> 
      ioError (userError (concat errs ++ usageInfo header progOpts))
  simpleHTTP nullConf {port=(optPort opts)} [
    dir "req" [withRequest $ ok . toResponse . show],
    dir "getpost" [withRequest getpost],
    dir "dopost" [withRequest dopost],
    withRequest rootOrServe]
