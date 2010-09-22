import Control.Applicative
import Control.Monad.Random
import Data.Maybe
import Happstack.Server
import Happstack.Server.FastCGI
import Network.FastCGI
import System.IO
import Text.XHtml
import Text.XHtml.Transitional

import Charism
import WordOrder
import qualified Data.ByteString.Char8 as B
import qualified Data.Trie as T

bothond f (a, b) = (f a, f b)

tileInfo pts num ltr = error "todo"

langToLexF = [
  langInfo "en" "/usr/share/dict/game/en.def" [
    --tileInfo 0 2 "_",
    tileInfo 1 12 "E",
    tileInfo 1 9 "A",
    tileInfo 1 9 "I",
    tileInfo 1 8 "O",
    tileInfo 1 6 "N",
    tileInfo 1 6 "R",
    tileInfo 1 6 "T",
    tileInfo 1 4 "L",
    -- double S since ppl tend to hold them
    --tileInfo 1 4 "S",
    tileInfo 1 8 "S",
    tileInfo 1 4 "U",
    tileInfo 2 4 "D",
    tileInfo 2 3 "G",
    tileInfo 3 2 "B",
    tileInfo 3 2 "C",
    tileInfo 3 2 "M",
    tileInfo 3 2 "P",
    tileInfo 4 2 "F",
    tileInfo 4 2 "H",
    tileInfo 4 2 "V",
    tileInfo 4 2 "W",
    tileInfo 4 2 "Y",
    tileInfo 5 1 "K",
    tileInfo 8 1 "J",
    tileInfo 8 1 "X",
    tileInfo 10 1 "Q",
    tileInfo 10 1 "Z"
    ],
  langInfo "de" "/usr/share/dict/game/de.def" [
    --tileInfo 0 2 "_",
    tileInfo 1 15 "E",
    tileInfo 1 9 "N",
    tileInfo 1 7 "S",
    tileInfo 1 6 "I",
    tileInfo 1 6 "R",
    tileInfo 1 6 "T",
    tileInfo 1 6 "U",
    tileInfo 1 5 "A",
    tileInfo 1 4 "D",
    tileInfo 2 4 "H",
    tileInfo 2 3 "G",
    tileInfo 2 3 "L",
    tileInfo 2 3 "O",
    tileInfo 3 4 "M",
    tileInfo 3 2 "B",
    tileInfo 3 1 "W",
    tileInfo 3 1 "Z",
    tileInfo 4 2 "C",
    tileInfo 4 2 "F",
    tileInfo 4 2 "K",
    tileInfo 4 1 "P",
    tileInfo 6 1 "Ä",
    tileInfo 6 1 "J",
    tileInfo 6 1 "Ü",
    tileInfo 6 1 "V",
    tileInfo 8 1 "Ö",
    tileInfo 8 1 "X",
    tileInfo 10 1 "Q",
    tileInfo 10 1 "Y"
    ]
  ]

ltrs :: String
ltrs = concat . map (\ (i, c) -> replicate c . chr $ i + ord 'A') $
  zip [0..] ltrCnts

main :: IO ()
main = do
  ls <- liftIO $
    map (bothond B.unpack . B.break (== ' ')) . B.lines <$>
    B.readFile lexFNWithRecDefs
  let
    wdLen = 7
    lenReq l = l >= 3 && l <= wdLen
    lex = T.fromList $ filter (lenReq . length . fst) ls
  runFastCGI . serverPartToCGI . withRequest $ \ req -> do
    wds <- liftIO . evalRandIO $ genWds lex Rand =<< genRack lex ltrs wdLen
    let wdDefs = map (\ x -> x ++ " " ++ fromJust (flip T.lookup lex x)) wds
    ok . toResponse $ show [wds, map (showHtmlFragment . toHtml) wdDefs]

