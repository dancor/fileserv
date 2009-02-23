{-# OPTIONS -fglasgow-exts #-}
module HTTP.FileServe (fileServe') where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans
import Data.List
import Data.Maybe
import HAppS.Server.SimpleHTTP
import System.Directory
import System.IO
import System.Locale(defaultTimeLocale)
import System.Log.Logger
import System.Time -- (formatCalendarTime, toUTCTime,TOD(..))
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as Map
import qualified HAppS.Server.SimpleHTTP as SH

-- | Serve files with a mime type map under a directory.
--   Uses the function to transform URIs to FilePaths.
fileServe' localpath fdir mime rq = do
    let fp2 = takeWhile (/=',') fp
        fp = filepath
        safepath = filter (\x->not (null x) && head x /= '.') (rqPaths rq)
        filepath = intercalate "/"  (localpath:safepath)
        fp' = if null safepath then "" else last safepath
    if "TESTH" `isPrefixOf` fp'
        then renderResponse mime rq $ fakeFile $ read $ drop 5 $ fp'
        else do
    fe <- liftIO $ doesFileExist fp
    fe2 <- liftIO $ doesFileExist fp2
    de <- liftIO $ doesDirectoryExist fp
    -- error $ "show ilepath: " ++show (fp,de)
    let status | de   = "DIR"
               | fe   = "file"
               | fe2  = "group"
               | True = "NOT FOUND"
    liftIO $ logM "HAppS.Server.HTTP.FileServe" INFO ("fileServe: "++show fp++" \t"++status)
    if de then fdir mime rq fp else do
    getFile mime fp >>= flip either (renderResponse mime rq)
                (const $ returnGroup localpath mime rq safepath)

-- if fp has , separated then return concatenation with content-type of last
-- and last modified of latest
tr a b = map $ \ x -> if x == a then b else x
ltrim = dropWhile $ flip elem " \t\r"

returnGroup localPath mime rq fp = do
  let fps0 = map ((:[]) . ltrim) . lines . tr ',' '\n' $ last fp
      fps = map (intercalate "/" . ((localPath:init fp) ++)) fps0
  mbFiles <- mapM (getFile mime) $ fps
  let notFounds = [x | Left x <- mbFiles]
      files = [x | Right x <- mbFiles]
  if not $ null notFounds
    then fileNotFound $ drop (length localPath) $ head notFounds else do
  let totSize = sum $ map (snd . fst) files
      maxTime = maximum $ map (fst . fst) files :: ClockTime

  renderResponse mime rq ((maxTime,totSize),(fst $ snd $ head files,
                                             L.concat $ map (snd . snd) files))

fileNotFound fp = do setResponseCode 404
                     return $ toResponse $ "File not found "++ fp

fakeFile fakeLen = ((TOD 0 0,L.length body),("text/javascript",body))
    where
      body = L.pack $ (("//"++(show len)++" ") ++ ) $ (take len $ repeat '0') ++ "\n"
      len = fromIntegral fakeLen

getFile mime fp = do
  let ct = Map.findWithDefault "text/plain" (getExt fp) mime
  fe <- liftIO $ doesFileExist fp
  if not fe then return $ Left fp else do

  time <- liftIO  $ getModificationTime fp
  h <- liftIO $ openBinaryFile fp ReadMode
  size <- liftIO $ hFileSize h
  lbs <- liftIO $ L.hGetContents h
  return $ Right ((time,size),(ct,lbs))

renderResponse mime rq ((modtime,size),(ct,body)) = do

  let notmodified = getHeader "if-modified-since" rq == Just (P.pack $ repr)
      repr = formatCalendarTime defaultTimeLocale
             "%a, %d %b %Y %X GMT" (toUTCTime modtime)
  -- "Mon, 07 Jan 2008 19:51:02 GMT"
  -- when (isJust $ getHeader "if-modified-since"  rq) $ error $ show $ getHeader "if-modified-since" rq
  if notmodified then do setResponseCode 304 ; return $ toResponse "" else do
  let mod = getHeader "if-modified-since" rq
  modifyResponse (setHeader "HUH" $ show $ (fmap P.unpack mod == Just repr,mod,Just repr))
  modifyResponse (setHeader "Last-modified" repr)
  -- if %Z or UTC are in place of GMT below, wget complains that the last-modified header is invalid
  modifyResponse (setHeader "Content-Length" (show size))
  modifyResponse (setHeader "Content-Type" ct)
  return $ resultBS 200 body

getExt fPath = reverse $ takeWhile (/='.') $ reverse fPath
