{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Random
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Console.GetOpt
import System.IO
import System.Random
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS

--import WordOrder
import qualified Data.Trie as T

type Lex a = T.Trie Char a

subseq :: Eq a => [a] -> [a] -> Bool
[] `subseq` _ = True
_ `subseq` [] = False
(x:xs) `subseq` (y:ys) = if x == y then xs `subseq` ys else (x:xs) `subseq` ys

main :: IO ()
main = do
    shortWds <- map (first DT.unpack) . HMS.toList . HMS.fromListWith (++) .
        map (\w -> (DT.pack . sort $ DT.unpack w, [w])) .
        DT.lines <$> DTI.readFile "/home/danl/data/enwikt-es-wds-3-to-6.txt"
    sevLtrWds <- map (first DT.unpack . second sort) . HMS.toList .
        HMS.fromListWith (++) .
        map (\w -> (DT.pack . sort $ DT.unpack w, [w])) .
        DT.lines <$> DTI.readFile "/home/danl/data/enwikt-es-wds-7.txt"
    let computeShorts ltrs = sortBy (comparing DT.length <> compare) $ concat
            [wds | (shortLtrs, wds) <- shortWds, shortLtrs `subseq` ltrs]
        myShow = DT.intercalate "," . map (\w -> "\"" <> w <> "\"")
    DTI.putStrLn . (\x -> "var lolPickWordsArr=[" <> x <>
        "];function pickWords(){return lolPickWordsArr[" <>
        "Math.floor(Math.random()*lolPickWordsArr.length)];}") . 
        DT.intercalate "," . map (\l -> "[" <> l <> "]") $
        map (\(a,b) -> myShow $ computeShorts a ++ b) sevLtrWds

--allRacks = 

{-
genWds :: (RandomGen g) => Lex a -> WordOrder -> String -> Rand g [String]
genWds lex wdOrd wd = case wdOrd of
  Alph -> return $ L.sortBy (comparing length `mappend` compare) wds
  Rand -> do
    -- random letter ordering to pseudoalphabetize answers
    charToRand <- M.fromList . zip (L.nub wd) <$> getRandomInts
    return $ L.sortBy (comparing length `mappend`
      comparing (map $ fromJust . flip M.lookup charToRand)) wds
  where wds = allWds lex wd

askWds :: Lex a -> Int -> WordOrder -> IO ()
askWds lex wdLen wdOrd = do
  let myLtrs = concat $ replicate 4 "abcdefghijklmnopqrstuvwxyz"
  wd <- evalRandIO $ genRack lex myLtrs wdLen
  wds <- evalRandIO $ genWds lex wdOrd wd
  putStr "\027[2J"
  hFlush stdout
  askWd wd wd wds [] "" Nothing True
  askWds lex wdLen wdOrd
-}

splitOut :: [x] -> Int -> (x, [x])
splitOut xs i = (head p2, p1 ++ tail p2) where (p1, p2) = splitAt i xs

splitOuts :: [x] -> [(x, [x])]
splitOuts xs = map (splitOut xs) $ take (length xs) [0..]

allWds :: Lex a -> [Char] -> [String]
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
allFullWds :: Lex a -> [Char] -> [String]
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
mapMap m = map (\ k -> fromJust $ M.lookup k m)

funOrd :: M.Map Char Int -> String -> String -> Ordering
funOrd mapOrd a b = if aN > bN then GT else if aN < bN then LT else
    compare (mapMap mapOrd a) (mapMap mapOrd b)
  where
    aN = length a
    bN = length b

replPos :: Int -> [a] -> a -> [a]
replPos i xs x = take i xs ++ [x] ++ drop (i + 1) xs

squashList :: Int -> Int -> [String] -> [String]
squashList width wdLen wds = map (intercalate " ") $ chunksOf wdsPerLine wds
  where
    wdsPerLine = ceiling (fromIntegral (width + 1) / fromIntegral (wdLen + 1))

{-
askWd :: String -> String -> [String] -> [String] -> String -> Maybe String ->
  Bool -> IO ()
askWd wdOrig wd wds wdsGot wdPartial wdWrongMby fullDraw = do
  let
    lenToWds = M.fromListWith (++) $ map renderWd wds
    renderWd w = (wL, [if w `elem` wdsGot then w else replicate wL '.'])
      where
      wL = length w
    maxHeight = 25
    n = length wds
    charWidth = ceiling (fromIntegral
      (n + (sum $ map (\ (i, ws) -> i * length ws) $ M.toList lenToWds)) /
      fromIntegral (maxHeight + 1 - length (M.toList lenToWds))) - 1
    list = intercalate "\n" $ L.intercalate [[]] $
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
      ( fst $ runRand (randShuffle wd) g
      , wdPartial
      , wdsGot
      , Nothing
      , False
      , False
      )
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
-}
