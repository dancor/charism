{-# OPTIONS_GHC -O2 #-}

module Main where

import Control.Applicative
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Random
import Data.Char
import Data.Maybe
import FUtil
import System
import System.Console.GetOpt
import System.IO
import System.Random
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Trie as T

type Lex = T.Trie Char ()

data Flag = WdLen String | WdLenMin String | WdOrd String
options :: [OptDescr Flag]
options = [
  Option ['n'] ["num"] (ReqArg WdLen "wdLen") "word length max",
  Option ['m'] ["min"] (ReqArg WdLenMin "wdLenMin") "word length min",
  Option ['o'] ["order"] (ReqArg WdLen "a|r") "word order (alph|rand)"]
type OptVals = (Int, Int, String)
-- FIXME: change to funcl
procOpt :: Flag -> OptVals -> OptVals
procOpt s (n, m, o) = case s of
  WdLen n' -> (read n', m, o)
  WdLenMin m' -> (n, read m', o)
  WdOrd o' -> if o' `elem` ["a", "r"]
    then (n, m, read o')
    else error "invalid word order"

lexFN :: String
lexFN = "/usr/share/dict/scrabble"

ltrCnts :: [Int]
ltrCnts = [
  9, 2, 2, 4, 14, 2,  -- A - F
  3, 2, 9, 1, 1, 4, 2,  -- G - M
  6, 8, 2, 1, 6, 8{-4-},  -- N - S   -- double s's since ppl tend to hold them
  6, 4, 2, 2, 1, 2, 1]  -- T - Z

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

ltrs :: String
ltrs = concat . map (\ (i, c) -> replicate c . chr $ i + ord 'A') $
  enumerate ltrCnts

-- pull tiles out of bag that happen to make words
ltrWds :: Lex -> Int -> Rand StdGen [String]
ltrWds lex wdLen = do
  -- TODO more concise?
  wd <- take wdLen <$> shuffle ltrs
  (case allFullWds lex wd of
    [] -> id
    _ -> (wd:)
    ) <$> ltrWds lex wdLen

splitOut :: [x] -> Int -> (x, [x])
splitOut xs i = (head p2, p1 ++ tail p2) where (p1, p2) = splitAt i xs

splitOuts :: [x] -> [(x, [x])]
splitOuts xs = map (splitOut xs) $ take (length xs) [0..]

allWds :: Lex -> [Char] -> [String]
allWds (T.Trie (Just _) _) [] = [""]
allWds (T.Trie Nothing _)  [] = []
allWds (T.Trie wdMby rem)  cs = case wdMby of
    Nothing -> r
    Just _  -> [[]] ++ r
  where
    -- determine letters which can possibly continue, and try each one
    r = L.nub $ concat $ map (\ (c, cs_rem) ->
      case M.lookup c rem of
        Nothing   -> []
        Just trie -> [[c] ++ wd | wd <- allWds trie cs_rem]
      ) $ splitOuts cs

-- or just filter allWds but might be slower (eh prob'd be fine); check later?
allFullWds :: Lex -> [Char] -> [String]
allFullWds (T.Trie (Just _) _) [] = [""]
allFullWds (T.Trie Nothing _)  [] = []
allFullWds (T.Trie _ rem)  cs = r
  where
    -- determine letters which can possibly continue, and try each one
    r = L.nub $ concat $ map (\ (c, cs_rem) ->
      case M.lookup c rem of
        Nothing   -> []
        Just trie -> [[c] ++ wd | wd <- allFullWds trie cs_rem]
      ) $ splitOuts cs

mapMap :: Ord k => M.Map k v -> [k] -> [v]
mapMap m = map (\k -> fromJust $ M.lookup k m)

funOrd :: M.Map Char Int -> String -> String -> Ordering
funOrd mapOrd a b = if aN > bN then GT else if aN < bN then LT else
    compare (mapMap mapOrd a) (mapMap mapOrd b)
  where
    aN = length a
    bN = length b

replPos :: Int -> [a] -> a -> [a]
replPos i xs x = take i xs ++ [x] ++ drop (i + 1) xs

squashList :: Int -> Int -> [String] -> [String]
squashList width wdLen wds = map interwords $ splitN wdsPerLine wds where
  wdsPerLine = ceiling (fromIntegral (width + 1) / fromIntegral (wdLen + 1))

askWd :: String -> String -> [String] -> [String] -> String -> Maybe String ->
  Bool -> IO ()
askWd wdOrig wd wds wdsGot wdPartial wdWrongMby fullDraw = do
  let
    lenToWds = M.fromListWith (++) $ map renderWd wds where
      renderWd w = (wL, [if w `elem` wdsGot then w else replicate wL '.']) where
        wL = length w
    maxHeight = 25
    n = length wds
    charWidth = ceiling (fromIntegral
      (n + (sum $ map (\ (i, ws) -> i * length ws) $ M.toList lenToWds)) /
      fromIntegral (maxHeight + 1 - length (M.toList lenToWds))) - 1
    list = interlines $ L.intercalate [[]] $
      map (uncurry (squashList charWidth)) $ M.toList lenToWds
    wrongStr = if isNothing wdWrongMby || length (fromJust wdWrongMby) < 3
      then ""
      else "\"" ++ fromJust wdWrongMby ++ "\" is not a word"
    clL = "\027[K\n"
  putStr $ (if fullDraw
    then "\027[0;0H" ++ list ++ concat (replicate 4 clL)
    --then "\027[0;0H\n" ++ concatMap (show . ord) ("x" ++ wdPartial ++ "x")
    else "\027[2A\r") ++ (L.intersperse ' ' wd)
    ++ clL ++ wrongStr ++ clL ++ wdPartial ++ "\027[K\027[J"
  hFlush stdout
  c <- hGetChar stdin
  let
    cU = toUpper c
    delAct = if null wdPartial
      then return (wd, "", wdsGot, Nothing, False, False)
      else return (replPos (length $ takeWhile (/= '.') wd) wd $ last
        wdPartial, init wdPartial, wdsGot, Nothing, False, False)
  (wd', wdPartial', wdsGot', wdWrongMby', fullDraw', quit) <- case cU of
    ' '  -> newStdGen >>= \g -> return
      (fst $ runRand (shuffle wd) g, wdPartial, wdsGot, Nothing, False, False)
    '\n' -> if wdPartial `elem` wds
      then return (wdOrig, "", wdsGot ++ [wdPartial], Nothing, True, False)
      else putStr "\027[A" >>
        return (wdOrig, "", wdsGot, Just wdPartial, False, False)
    '?' -> return (wdOrig, "",
      S.toList $ (S.fromList wds) `S.difference` (S.fromList wdsGot),
      Nothing, True, False)
    '\027' -> return (wdOrig, "", wdsGot, Nothing, False, True)
    '\008' -> delAct
    '\DEL' -> delAct
    '\f' -> return (wd, wdPartial, wdsGot, Nothing, True, False)
    _    -> if cU /= '.' && cU `elem` wd
      then return (replPos (length $ takeWhile (/= cU) wd) wd '.',
	wdPartial ++ [cU], wdsGot, Nothing, False, False)
      else
        return (wd, wdPartial, wdsGot, Nothing, False, False)
  unless quit $ askWd wdOrig wd' wds wdsGot' wdPartial' wdWrongMby' fullDraw'

main :: IO ()
main = do
  args <- getArgs
  let
    header = "Usage:"
    (opts, a) = case getOpt Permute options args of
      (o, n, [])   -> (o, n)
      (_, _, errs) -> error $ concat errs ++ usageInfo header options
    (wdLen, wdLenMin, wdOrd) = foldr procOpt (7, 3, "a") opts
  print "loading lexicon"
  h <- openFile lexFN ReadMode
  c <- B.hGetContents h
  let
    ls = B.lines c
    lenReq = (\ x -> x >= wdLenMin && x <= wdLen) . length
    killR = filter (/= '\r')  -- TODO better here (BS func ya?)
    lex = T.fromList . map (flip (,) ()) . filter lenReq .
      map (killR . B.unpack) $ ls
  lex `seq` print "loaded lexicon"
  g <- getStdGen
  askWds g lex wdLen wdOrd

askWds :: StdGen -> Lex -> Int -> [Char] -> IO ()
askWds g lex wdLen wdOrd = do
  let
    (g', g'') = split g
    wd = head . fst $ runRand (ltrWds lex wdLen) g'
    -- random letter ordering to pseudoalphabetize answers
    ltrOrd = M.fromList . map swap . enumerate .
      -- why need reverse? (find that bug instead of doing this fix)
      (if wdOrd == "a"
        then L.reverse . L.sort
        else (\ l -> fst $ runRand (shuffle l) g')) $ L.nub wd
    wds = L.sortBy (funOrd ltrOrd) $ filter ((> 2) . length) $ allWds lex wd
  hSetBuffering stdin NoBuffering
  putStr "\027[2J"
  hFlush stdout
  askWd wd wd wds [] "" Nothing True
  askWds g'' lex wdLen wdOrd

{- GRAVEYARD

allSubPerms :: [x] -> [[x]]
allSubPerms xs = [[]] ++ (concat $ map (collate . splitOut xs) $ take (length xs) [0..])
  where
    collate :: (x, [x]) -> [[x]]
    collate (a, as) = [[a] ++ l | l <- allSubPerms as]

allPerms :: [x] -> [[x]]
allPerms [] = [[]]
allPerms xs = concat $ map (collate . splitOut xs) $ take (length xs) [0..]
  where
    collate :: (x, [x]) -> [[x]]
    collate (a, as) = [[a] ++ l | l <- allPerms as]

-}
