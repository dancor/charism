{-# OPTIONS_GHC -O2 #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Trie as T
import System
import System.Console.GetOpt
import System.IO
import System.Random
import Util

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

lexFN = "/usr/share/dict/scrabble"

ltrCnts :: [Int]
ltrCnts = [
  9, 2, 2, 4, 14, 2,  -- A - F
  3, 2, 9, 1, 1, 4, 2,  -- G - M
  6, 8, 2, 1, 6, 8{-4-},  -- N - S   -- double s's since ppl tend to hold them
  6, 4, 2, 2, 1, 2, 1]  -- T - Z

enumerate = zip [0..]

rep n x = take n $ repeat x

ltrs :: String
ltrs = concat $ map (\ (i, c) -> rep c $ chr $ i + ord 'A') $ enumerate ltrCnts

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

-- iterate throgh random selected letters until have word
findWd :: StdGen -> Lex -> Int -> String
findWd g lex wdLen = head $ dropWhile (null . allFullWds lex) $ wdList g where
  wdList :: StdGen -> [String]
  wdList g = [take wdLen $ shuffle g ltrs] ++ wdList g' where
    (g', g'') = split g

mapMap :: Ord k => M.Map k v -> [k] -> [v]
mapMap m = map (\k -> fromJust $ M.lookup k m)

funOrd :: M.Map Char Int -> String -> String -> Ordering
funOrd mapOrd a b = if aN > bN then GT else if aN < bN then LT else
    compare (mapMap mapOrd a) (mapMap mapOrd b)
  where
    aN = length a
    bN = length b

-- how is this not in prelude?
swap (a, b) = (b, a)

replPos i xs x = take i xs ++ [x] ++ drop (i + 1) xs

squashList :: Int -> Int -> [String] -> [String]
squashList width wdLen wds = map interwords $ splitN wdsPerLine wds where
  wdsPerLine = ceiling (fromIntegral (width + 1) / fromIntegral (wdLen + 1))

askWd :: String -> String -> [String] -> [String] -> String -> Maybe String ->
  Bool -> IO ()
askWd wdOrig wd wds wdsGot wdPartial wdWrongMby fullDraw = do
  let lenToWds = M.fromListWith (++) $ map renderWd wds where
        renderWd w = (wL, [if w `elem` wdsGot then w else rep wL '.']) where
          wL = length w
      maxHeight = 25
      n = length wds
      charWidth = ceiling (fromIntegral
        (n + (sum $ map (\ (i, ws) -> i * length ws) $ M.toList lenToWds)) /
        fromIntegral (maxHeight + 1 - length (M.toList lenToWds))) - 1
      list = interlines $ L.intercalate [[]] $
        map (uncurry (squashList charWidth)) $ M.toList lenToWds
  let
    wrongStr = if isNothing wdWrongMby || length (fromJust wdWrongMby) < 3
      then ""
      else "\"" ++ fromJust wdWrongMby ++ "\" is not a word"
    clL = "\027[K\n"
  putStr $ (if fullDraw
    then "\027[0;0H" ++ list ++ concat (rep 4 clL)
    --then "\027[0;0H\n" ++ concatMap (show . ord) ("x" ++ wdPartial ++ "x")
    else "\027[2A\r") ++ (L.intersperse ' ' wd)
    ++ clL ++ wrongStr ++ clL ++ wdPartial ++ "\027[K\027[J"
  hFlush stdout
  c <- hGetChar stdin
  let c' = toUpper c
  (wd', wdPartial', wdsGot', wdWrongMby', fullDraw', quit) <- case c' of
    ' '  -> newStdGen >>= \g -> return
      (shuffle g wd, wdPartial, wdsGot, Nothing, False, False)
    '\n' -> if wdPartial `elem` wds
      then return (wdOrig, "", wdsGot ++ [wdPartial], Nothing, True, False)
      else putStr "\027[A" >>
        return (wdOrig, "", wdsGot, Just wdPartial, False, False)
    '?' -> return (wdOrig, "",
      S.toList $ (S.fromList wds) `S.difference` (S.fromList wdsGot),
      Nothing, True, False)
    '\027' -> return (wdOrig, "", wdsGot, Nothing, False, True)
    -- hack, shouldn't be echoing to begin with right:
    '\008' -> when (null wdPartial) (putStrLn "") >> if null wdPartial
      then return (wd, "", wdsGot, Nothing, False, False)
      else return (replPos (length $ takeWhile (/= '.') wd) wd $ last
        wdPartial, init wdPartial, wdsGot, Nothing, False, False)
    _    -> if c' /= '.' && c' `elem` wd
      then return (replPos (length $ takeWhile (/= c') wd) wd '.',
	wdPartial ++ [c'], wdsGot, Nothing, False, False)
      else
        return (wd, wdPartial, wdsGot, Nothing, False, False)
  unless quit $ askWd wdOrig wd' wds wdsGot' wdPartial' wdWrongMby' fullDraw'

main :: IO ()
main = do
  args <- getArgs
  (opts, a) <- case getOpt Permute options args of
    (o, n, [])   -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++
      usageInfo header options)) where header = "Usage:"
  let (wdLen, wdLenMin, wdOrd) = foldr procOpt (7, 3, "a") opts
  print "loading lexicon"
  h <- openFile lexFN ReadMode
  c <- B.hGetContents h
  let ls = B.lines c
      lenReq = (\ x -> x >= wdLenMin && x <= wdLen) . length
      lex = T.fromList . map (flip (,) ()) . filter lenReq . map B.unpack $ ls
  -- does this do anything?  like speed up by loading into memory
  evaluate lex
  print "loaded lexicon"
  g <- getStdGen
  askWds g lex wdLen wdOrd

askWds g lex wdLen wdOrd = do
  let (g', g'') = split g
      wd = findWd g' lex wdLen
      -- random letter ordering to pseudoalphabetize answers
      ltrOrd = M.fromList $ map swap $ enumerate $
        -- why need reverse? (find that bug instead of doing this fix)
        (if wdOrd == "a" then L.reverse . L.sort else shuffle g') $ L.nub wd
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
