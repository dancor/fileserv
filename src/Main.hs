import Control.Monad.Trans
import Data.List
import Data.Maybe
import HAppS.Server
import HTTP.FileServe
import System.Directory
import System.Environment
import System.FilePath
import System.Console.GetOpt
import Text.XHtml hiding (dir)
import qualified Data.Map as Map

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

myMimeTypes = Map.insert "mem" "text/plain; charset=utf-8" mimeTypes

rootOrServe req =  if rqPaths req == []
  then fileServe' "index.html" fdir myMimeTypes req
  else fileServe' "." fdir myMimeTypes req where
      fdir _mime _rq fp = do
        contents <- liftIO $ getDirectoryContents fp
        return $ toResponse $ dirify fp $ filter (`notElem` ["."] ++
          (if fp == "." then [".."] else [])) $ contents

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
    withRequest rootOrServe]
