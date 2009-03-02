import Chess
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
import HAppS.Server hiding (method)
import HTTP.FileServe
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

myMimeTypes :: M.Map [Char] [Char]
myMimeTypes = M.fromList [
  ("svg", "image/svg+xml; charset=utf-8"),
  ("mem", "text/plain; charset=utf-8")
  ] `M.union` mimeTypes

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
  in ok . toResponse $ concatHtml [
    toHtml "posting..",
    form hids ! [method "POST", action origAction],
    script $ toHtml "document.forms[0].submit()"
    ]

readFen :: String -> Either String Game
readFen fen = do
  let
    readFenCh :: Char -> Either String [BdSq]
    readFenCh x = case readMb [x] of
      Just i -> return $ replicate i Emp
      _ -> if xUp `elem` "PNBRQK"
        then return [HasP (if x == xUp then CW else CB) xUp]
        else throwError $ "unknown piece type" ++ [xUp]
        where xUp = toUpper x
  [fenBd, fenColor, fenCastle, fenPassant, _fenHalfMvClock, _fenMv] <- do
    let
      fenParts = breaks (== ' ') fen
      fenPartsLen = length fenParts
    if fenPartsLen >= 1 && fenPartsLen <= 6
      then return $ fenParts ++ drop fenPartsLen ["", "w", "KQkq", "-", "", ""]
      else throwError "fen syntax"
  bd <- mapM ((concat <$>) . mapM readFenCh) $ breaks (== '/') fenBd
  let bdH = length bd
  when (bdH == 0) $ throwError "board cannot have height zero"
  let bdW = length (bd !! 0)
  color <- case fenColor of
    "w" -> return CW
    "b" -> return CB
    _ -> throwError $
      "fen \"active color\" should be \"w\" or \"b\", but got: " ++ fenColor
  let
    canCastle = M.fromList [
      (CW, S.fromList $ filter isLower fenCastle),
      (CB, S.fromList . map toLower $ filter isUpper fenCastle)
      ]
  lastPawn2 <- case fenPassant of
    "-" -> return Nothing
    c -> do
      let (iMb, jMb) = evalState parseCoord c
      i <- maybe
        (throwError $ "could not read x-coord of enPassant info: " ++ c)
        return iMb
      j <- maybe
        (throwError $ "could not read y-coord of enPassant info: " ++ c)
        return jMb
      if i >= 1 && i <= bdW && j >= 1 && j <= bdH
        then return $ Just (i :: Int, j :: Int)
        else throwError $
          "fen en passant information off board: " ++ fenPassant
  return $ Game {
    gmBd = Bd $ listArray ((1, 1), (bdW, bdH)) $ concat bd,
    gmTurn = color,
    gmLastPawn2 = lastPawn2,
    gmCanCastle = canCastle,
    gmHist = []
    }

fenStart :: String
fenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

ch :: (Monad m) => Request -> WebT m Response
ch req = ok $ toResponse resp where

  inps = procQuery $ rqInputs req
  fen = fromMaybe fenStart $ lookup "fen" inps
  useSvg = isJust $ lookup "svg" inps
  w = fromMaybe "45" $ lookup "sqw" inps
  h = fromMaybe w $ lookup "sqh" inps
  moves = filter (not . all (\ x -> isDigit x || x == '.')) . words .
    fromMaybe "" $ lookup "pgn" inps

  resp = case readFen fen of
    Right gm -> case foldM (flip doMvStrPure) gm moves of
      Right gm' -> renderGm gm'
      Left err -> toHtml err
    Left err -> toHtml err
  renderGm :: Game -> Html
  renderGm gm = (table ! [border 0, cellpadding 0, cellspacing 0]) . toHtml .
    aboves . map besides . splitN bdW . map (\ ((i, j), p) -> td ! [
      width w, height h, align "center",
      strAttr "bgcolor" $ if (i + j) `mod` 2 == 0 then "#ff7" else "#770"
      ] $ renderBdSq p) $ assocs bd
    where
    Bd bd = gmBd gm
    (bdW, _bdH) = bdBounds $ gmBd gm
  renderBdSq :: BdSq -> Html
  renderBdSq Emp = noHtml
  renderBdSq (HasP CW p) = svg $ "w" ++ [toLower p]
  renderBdSq (HasP CB p) = svg $ "b" ++ [toLower p]
  svg s = if useSvg
    then object ! [
      thetype "image/svg+xml",
      strAttr "data" $ "../img/g/ch/" ++ s ++ ".svg"
      ] $ toHtml "svg unsupported?"
    else image ! [src $ "../img/g/ch/" ++ s ++ ".png"]

tagsToHtml :: [Tag] -> String
tagsToHtml = concatMap tagToHtml where

  tagToHtml :: Tag -> String
  tagToHtml (TagOpen name attrs) = "<" ++ name ++
    (if null attrs then "" else " " ++ interwords (map showAttr attrs)) ++ ">"
  tagToHtml (TagClose s) = "</" ++ s ++ ">"
  tagToHtml (TagText s) = s
  tagToHtml _ = ""

  showAttr :: (String, String) -> String
  showAttr (name, val) =
    name ++
    (if not (null name) && not (null val) then "=" else "") ++
    (if null val then "" else "\"" ++ concatMap escQ val ++ "\"")

  escQ :: Char -> [Char]
  escQ '"' = "&quot;"
  escQ '&' = "&amp;"
  escQ c = [c]

changeFirst :: (a -> Maybe a) -> [a] -> [a]
changeFirst _ [] = []
changeFirst f (x:xs) = case f x of
  Nothing -> x : changeFirst f xs
  Just x' -> x' : xs

hl :: (MonadIO m) => Request -> WebT m Response
hl req = do
  let
    inps = procQuery $ rqInputs req
    url = fromMaybe "about:blank" $ lookup "url" inps
    after = fromMaybe "" $ lookup "after" inps
  resp <- case parseURI url of
    Nothing -> return $ toHtml $ "Could not parse url: " ++ show url
    Just uri -> do
      httpRes <- liftIO . H.simpleHTTP $ H.Request uri H.GET [] ""
      case httpRes of
        Left err -> return $ toHtml $ "Http request error: " ++ show err
        Right urlResp -> do
          let
            tags = parseTags $ H.rspBody urlResp
            tagsRel = map (fixRel uri) tags
            tagsAfter = flip changeFirst tagsRel $ \ t -> case t of
              TagText s -> if after `isInfixOf` s'
                then Just $ TagText $
                  subst after (after ++ "<br><a name=\"lol\"></a>") s'
                else Nothing
                where s' = map head $ groupBy (\ x y -> x == y && isSpace x) s
              _ -> Nothing
          return $ primHtml $ tagsToHtml tagsAfter
  ok $ toResponse resp

fixRel :: URI -> Tag -> Tag
fixRel u (TagOpen name attrs) = TagOpen name $ map (fixRelAttr u) attrs where
  fixRelAttr :: URI -> Attribute -> Attribute
  fixRelAttr u a@(name, val) = if map toLower name == "src"
    then case parseURIReference val >>= (`relativeTo` u) of
      Nothing -> a
      Just valUri -> (name, show valUri)
    else a
fixRel u t = t

hlside :: (Monad m) => Request -> WebT m Response
hlside _ = ok $ toResponse resp where
  resp = toHtml "hi"

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
    dir "ch" [withRequest ch],
    dir "hl" [withRequest hl],
    dir "hlside" [withRequest hlside],
    dir "getpost" [withRequest getpost],
    dir "dopost" [withRequest dopost],
    withRequest rootOrServe]
