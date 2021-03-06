module Chess where

import Control.Applicative
import Control.Arrow
import Control.Monad.Error
import Control.Monad.State
import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import FUtil
import qualified Data.Map as M
import qualified Data.Set as S

bdW, bdH :: Int
bdW = 8
bdH = 8

data Color = CW | CB
  deriving (Eq, Ord, Show)
type Piece = Char
data BdSq = Emp | HasP Color Piece
  deriving (Eq, Show)
type Pt = (Int, Int)
data Bd = Bd (Array Pt BdSq)
  deriving (Eq, Show)
data Game = Game {
  gmBd :: Bd,
  gmTurn :: Color,
  gmLastPawn2 :: Maybe Pt,
  gmCanCastle :: M.Map Color (S.Set Piece),
  gmHist :: [String]
  }
  deriving (Eq, Show)
data Move = Move {
  fromP :: Maybe Piece,
  fromX :: Maybe Int,
  fromY :: Maybe Int,
  isATake :: Bool,
  toX :: Maybe Int,
  toY :: Maybe Int,
  promote :: Maybe Piece
  } |
  Castle Piece
  deriving (Eq, Show)

initGm :: Game
initGm = Game {
  gmBd = Bd $ listArray ((1, 1), (bdW, bdH)) $ b ++ empzone ++ w,
  gmTurn = CW,
  gmLastPawn2 = Nothing,
  gmCanCastle = M.fromList [(c, S.fromList "qk") | c <- [CW, CB]],
  gmHist = []
  } where
  backrow = "RNBQKBNR"
  frontrow = replicate bdW 'P'
  b = map (HasP CB) $ backrow ++ frontrow
  empzone = replicate (bdW * (bdH - 4)) Emp
  w = map (HasP CW) $ frontrow ++ backrow

modRet :: (MonadState t1 t) => (t1 -> (t2, t1)) -> t t2
modRet f = do
  g <- get
  let (x, g') = f g
  put g'
  return x

pChs, xChs, yChs :: String
pChs = "RNBQKP"
xChs = "abcdefgh"
yChs = "12345678"

tryFrom :: String -> String -> (Maybe Char, String)
tryFrom chs s = if null s then (Nothing, s) else let s1:sRest = s in
  if s1 `elem` chs then (Just s1, sRest) else (Nothing, s)

parseCoord :: State String (Maybe Int, Maybe Int)
parseCoord = do
  let
    parseX x = ord x - ord 'a' + 1
    parseY y = bdH - ord y + ord '1'
  x <- (parseX <$>) <$> modRet (tryFrom xChs)
  y <- (parseY <$>) <$> modRet (tryFrom yChs)
  return (x, y)

parseMv :: String -> Move
parseMv mvStr = case mvStr of
  "O-O" -> Castle 'K'
  "O-O-O" -> Castle 'Q'
  _ -> flip evalState (filter (`elem` pChs ++ xChs ++ yChs ++ "x") mvStr) $ do
    fromP <- modRet (tryFrom pChs)
    (x1, y1) <- parseCoord
    isATake <- isJust <$> modRet (tryFrom "x")
    -- could be more strict
    modRet (tryFrom "=")
    promote <- modRet (tryFrom pChs)
    g <- get
    if null g && not isATake
      then return $ Move {
        fromP = fromP,
        fromX = Nothing,
        fromY = Nothing,
        isATake = isATake,
        toX = x1,
        toY = y1,
        promote = promote
        }
      else do
        (x2, y2) <- parseCoord
        modRet (tryFrom "=")
        promote <- modRet (tryFrom pChs)
        return $ Move {
          fromP = fromP,
          fromX = x1,
          fromY = y1,
          isATake = isATake,
          toX = x2,
          toY = y2,
          promote = promote
          }

-- todo: castling, enpassant
-- - could be totally in eng..?

onBd :: Game -> (Int, Int) -> Bool
onBd gm (y, x) = y >= 1 && y <= bdH && x >= 1 && x <= bdW where
  (bdW, bdH) = bdBounds $ gmBd gm

bdBounds :: Bd -> (Int, Int)
bdBounds (Bd bd) = snd $ bounds bd

-- We cheat and return more squares than may actually be legal.
-- The engine does validation; we just need enough to do move disambiguation.
-- todo: We _do_ have to worry about exposed check for disambig unfortunately.
--       (but extremely rarely; I think crafty/xboard fuck this up actually!)
sqCanGetTo :: (Int, Int) -> Game -> Bool -> [(Int, Int)]
sqCanGetTo (y, x) gm isATake = let
  Bd bd = gmBd gm
  HasP turn p = bd ! (y, x)
  tryDir = tryDirPos (y, x)
  tryDirPos _ _ 0 dir = []
  tryDirPos (yPos, xPos) takeAll dist dir = let
    pos' = case dir of
      0 -> (yPos, xPos + 1)
      1 -> (yPos + 1, xPos + 1)
      2 -> (yPos + 1, xPos)
      3 -> (yPos + 1, xPos - 1)
      4 -> (yPos, xPos - 1)
      5 -> (yPos - 1, xPos - 1)
      6 -> (yPos - 1, xPos)
      7 -> (yPos - 1, xPos + 1)
    in if onBd gm pos'
      then case bd ! pos' of
        Emp -> (if isATake && not takeAll then id else (pos':)) $
          tryDirPos pos' takeAll (dist - 1) dir
        _ -> if isATake then [pos'] else []
      else []
  in case p of
    'K' -> concatMap (tryDir False 1) [0..7]
    'Q' -> concatMap (tryDir False 8) [0..7]
    'R' -> concatMap (tryDir False 8) [0, 2, 4, 6]
    'B' -> concatMap (tryDir False 8) [1, 3, 5, 7]
    'N' -> filter (onBd gm) $
      [(y + oy, x + ox) | oy <- [-2, 2], ox <- [-1, 1]] ++
      [(y + oy, x + ox) | oy <- [-1, 1], ox <- [-2, 2]]
    'P' -> if isATake
      then concatMap (tryDir True 1) (if turn == CW then [5, 7] else [1, 3])
      else tryDir False 2 (if turn == CW then 6 else 2)

-- one or more of x1, y1 could be missing
-- todo: when ambiguous, we should error instead of picking one?
-- -- engine gets that for us
resolveMv :: Game -> Move -> Either String Move
resolveMv gm mv0 = let
  -- when start spot is omitted, we will know the piece
  fillP mv = case fromP mv of
    Just _ -> mv
    Nothing -> mv {fromP = Just 'P'}

  Bd bd = gmBd gm
  tryXY :: ((Int, Int), BdSq) -> Move -> Move
  tryXY ((y, x), HasP turn p) mv = let
    eqOrN x yMbF = case yMbF mv of
      Just y -> x == y
      Nothing -> True
    in if turn == gmTurn gm &&
      eqOrN x fromX && eqOrN y fromY && p == fromJust (fromP mv) &&
      (fromJust $ toY mv, fromJust $ toX mv) `elem`
      sqCanGetTo (y, x) gm (isATake mv)
      then mv {fromX = Just x, fromY = Just y}
      else mv
  tryXY _ mv = mv
  in case mv0 of
    Move {toX = Just _, toY = Just _} -> Right $
      if isJust (fromX mv0) && isJust (fromY mv0)
        then mv0
        else foldr tryXY (fillP mv0) $ assocs bd
    Castle _ -> Right mv0
    _ -> Left "Could not resolve move"

isEmp :: BdSq -> Bool
isEmp Emp = True
isEmp _ = False

isPawn :: BdSq -> Bool
isPawn (HasP _ 'P') = True
isPawn _ = False

doMvPure :: String -> Move -> Game -> Either String Game
doMvPure mvStr mv gm = case mv of
  Move {fromX = Just x1, fromY = Just y1, toX = Just x2, toY = Just y2} ->
    doChanges . considerEnPassant . considerPromotion $ changes where
      changes = [
        ((y2, x2), bd ! (y1, x1)),
        ((y1, x1), Emp)
        ]
      considerEnPassant changes = if
        isPawn (bd ! (y1, x1)) && isEmp (bd ! (y2, x2)) && x1 /= x2
        then ((y1, x2), Emp):changes else changes
      considerPromotion changes = case promote mv of
        Nothing -> changes
        Just p -> onHead (second . const $ HasP (gmTurn gm) p) changes
  Castle p -> let
    c = gmTurn gm
    y = if c == CW then 8 else 1
    xKf = 5
    (xKt, xRf, xRt) = if p == 'K' then (7, 8, 6) else (3, 1, 4)
    in doChanges [
      ((y, xKf), Emp), ((y, xKt), HasP c 'K'),
      ((y, xRf), Emp), ((y, xRt), HasP c 'R')
      ]
  _ -> Left $ "Unknown move type: " ++ show mvStr ++ " " ++ show mv ++ " " ++
    show bd
  where
  Bd bd = gmBd gm
  doChanges changes = Right $ gm {
    gmBd = Bd $ bd // changes,
    gmTurn = if gmTurn gm == CW then CB else CW,
    gmHist = gmHist gm ++ [mvStr]
    }

eithErr :: (Error e, Monad m) => Either e a -> ErrorT e m a
eithErr = either throwError return

doMvStrPure :: String -> Game -> Either String Game
doMvStrPure mvStr gm = do
  mv <- resolveMv gm $ parseMv mvStr
  doMvPure mvStr mv gm
