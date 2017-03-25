module Main where

import Control.Applicative
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Random
import Data.Char
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Random
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Charism
import qualified Data.Trie as T
import qualified Opt

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  (opts, args) <- Opt.getOpts (homeDir </> ".charism" </> "config") $
    "usage: ./charism [options]"
  when (not $ null args) $ print "lol"
  print "loading lexicon"
  print "todo"
  {-
  ls <- B.lines <$> (B.hGetContents =<< openFile lexFN ReadMode)
  let
    lenReq l = l >= Opt.minWordLen opts && l <= Opt.maxWordLen opts
    killR = filter (/= '\r')  -- TODO better here (BS func ya?)
    lex = T.fromList . map (flip (,) ()) . filter (lenReq . length) $
      map (killR . B.unpack) ls
  lex `seq` print "loaded lexicon"
  hSetBuffering stdin NoBuffering
  askWds lex (Opt.maxWordLen opts) (Opt.wordOrder opts)
  -}
